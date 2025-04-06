;;; navigation.scm --- "vertical" score navigation backend for GNU LilyPond

;; Copyright (c) 2025 Saul James Tobin

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(use-modules (srfi srfi-26))

;; Modified from lily-library.scm
(module-set! (resolve-module '(lily)) 'uniq-list
             (lambda* (lst #:optional (= equal?) #:key (reverse? #f))
               "Remove adjacent duplicates from @var{lst}. For sorted lists,
faster than @code{delete-duplicates}. Comparisons are performed using
@code{equal?} or optionally @var{=}. With @var{reverse?} non-@code{#f}, return
the result in reverse order (slightly more efficient)."
               (let ((result (fold (lambda (x acc)
                                     (if (null? acc)
                                         (list x)
                                         (if (= x (car acc))
                                             acc
                                             (cons x acc))))
                                   '() lst)))
                 (if reverse?
                     result
                     (reverse! result '())))))

(define*-public ((lexicographic-comparator-from-keys test #:key (= equal?)
                                                     . selectors) . args)
  "Make a general lexicographic comparator that returns the result of @var{test}
for the first non-@code{equal?} set of values selected by applying each of
@var{selectors} in turn across the comparator's arguments. With
@code{#:=}@tie{}@var{=}, use the provided @var{=} proc instead of @code{equal?}.

For example, @code{(lexicographic-comparator-from-keys < car cadr caddr #:= =)}
would yield a function appropriate for comparing LilyPond version numbers in
list-form (e.g. @code{(2 25 20)})."
  (let lp ((vals (map (car selectors) args))
           (selectors (cdr selectors)))
    (if (and (pair? selectors)
             (apply = vals))
        (lp (map (car selectors) args)
            (cdr selectors))
        (apply test vals))))

(define-public (write-file-to-subdir data dir name ext)
  "Write @var{data} to @file{@var{dir}/@var{name}.@var{ext}} relative to the
current input file."
  (let* ((parent-dir (dirname (or input-file-name name)))
         (subdir (string-append parent-dir "/" dir))
         (tmp (begin (mkdir-if-not-exist subdir)
                     (make-tmpfile subdir)))
         (fname (string-append subdir "/" (basename name) ext)))
    (write data tmp)
    (ly:progress "\nConverting to `~a'..." fname)
    (close-port-rename tmp fname)))

(define-public (selective-map-contexts proc ctx)
  "Apply @var{proc} to context @var{ctx}.  If @var{proc} returns @code{#f},
recurse over the children of @var{ctx}. Returns the tree of contexts where
@var{proc} was successfully applied."
  (or (proc ctx)
      (map (cute selective-map-contexts proc <>)
           (ly:context-children ctx))))

(define*-public (split-sequential-segments lst #:optional (comparator <))
  "Split @var{lst} into monotonic segments under @var{comparator}, reversing the
initial ordering of @var{lst}."
  (fold (lambda (x acc)
          (if (and (pair? acc)
                   (comparator (caar acc)
                               x))
              (cons (cons x (car acc))
                    (cdr acc))
              (cons (list x) acc)))
        '() lst))

(define*-public (apply-export-type-conversions arg)
  "Recursively apply type conversions to @var{arg} as defined in
global paramater @var{current-export-type-conversions}."
  (cond
   ((list? arg)
    (map apply-export-type-conversions arg))
   ((and=> (assoc arg (current-export-type-conversions) (lambda (a b) (b a)))
           cdr)
    => (compose apply-export-type-conversions (cute <> arg)))
   (else arg)))

(define*-public (merge-sorted-tables alist1 alist2 #:optional (comparator <))
  "Combine two alists @var{alist1} and @var{alist2}, where the values are lists
sorted under @var{comparator}. Merge the value lists for any keys common to both
alists. Order of keys is not preserved."
  (if (null? alist1)
      alist2
      (fold (lambda (ftable accum)
              (assoc-set! accum (car ftable)
                          (merge (assoc-get (car ftable) accum '())
                                 (cdr ftable)
                                 comparator)))
            alist1 alist2)))

((@@ (lily) translator-property-description)
 'navigationTable list? "Mapping between input locations and rhythmic positions
for every @code{rhythmic-event} in @code{Score} up to the current timestep.")
((@@ (lily) translator-property-description)
 'navigationExportProperties symbol-list? "Context properties to include in
exported navigation data.")

(define-public current-export-type-conversions (make-parameter '()))
(define-public score-id-alist (make-parameter (list)))
(define-public collated-nav-tables (make-parameter (list)))
(define-public book-score-count (make-parameter 0))

(define-public (finalize-nav-data! nav-table)
  "Process data collected in a @code{Score}'s @code{navigationTable}. Sort by
input location, tag with score-id, remove duplicate events, and format for
export. The processed data is stored in the book-level parameters
@code{score-id-alist} and @code{collated-nav-tables}. If data has been
processed for all scores in the book, write both parameters to the output file,
then clear @code{score-id-alist}."
  (ly:progress "\nCollating navigation data...")
  (let* ((score-index (length (score-id-alist)))
         ;; score-ids are sufficiently unique for editors to use a single global
         ;; lookup table across all projects
         (score-id (string->symbol
                    (format #f "~a-~d-~d" current-outfile-name score-index
                            ;; disambiguate books with same name
                            (hash input-file-name (expt 1024 3)))))
         (file-list (map car nav-table))
         (final-tables
          (map (compose
                (cute map! (match-lambda
                             (((filename . location) . data)
                              (cons* location score-id
                                     (apply-export-type-conversions data)))) <>)
                (cute uniq-list <> (comparator-from-key cdar equal?)
                      #:reverse? #t)
                ;; sort backwards so uniq-list doesn't need to call reverse!
                (cute sort-list! <>
                      (lexicographic-comparator-from-keys > cadar caddar))
                cdr)
               nav-table)))
    (score-id-alist (acons score-id file-list (score-id-alist)))
    (collated-nav-tables (merge-sorted-tables
                          (collated-nav-tables) (map cons file-list final-tables)
                          (lexicographic-comparator-from-keys < caar cadar)))
    (when (= score-index (1- (book-score-count)))
      ;; Write to file as soon as translation is done for the last score in the
      ;; book, so editor navigation refresh doesn't have to wait for typesetting
      (export-nav-data)
      ;; If some score in the book didn't generate nav data, this clause never
      ;; runs. We reset the value of one of the parameters here as a flag to
      ;; tell toplevel-book-handler that nav data was successfully exported at
      ;; the end of translation. If score-id-alist still has data after
      ;; typesetting, toplevel-book-handler knows to run export-nav-data.
      (score-id-alist #f))))

(define-public (export-nav-data)
  "Write the navigation data for the current book, accumulated in
@code{score-id-alist} and @code{collated-nav-tables}, to
@file{.nav/@var{bookname}.l}, relative to the input file location."
  (ly:progress "\nExporting navigation data for book `~a'..."
               current-outfile-name)
  (write-file-to-subdir `((by-score . ,(score-id-alist))
                          (by-input-file . ,(collated-nav-tables)))
                        ;; .l because the data is generic Scheme/LISP format
                        ;; Use a fixed subdirectory relative to the input file
                        ;; (not the output directory) so editors can find it.
                        ".nav" current-outfile-name ".l"))

(define-public ((record-origins-listener ctx) event)
  "Make a listener callback for context @var{ctx} that gathers each event's
input location. start and end moment, and values of each property in
@code{navigationExportProperties}. Collect the data under alist entries per
input filename in @code{Score}@tie{}level property @code{navigationTable}. Each
entry is of the form:
@code{((filename line char col) . (beg-moment end-moment . export-props))}"
  (let ((location (ly:event-property event 'origin #f)))
    (when location ; ignore synthetic events
      (let* ((fl-ln-ch-cl (ly:input-file-line-char-column location))
             (filename (car fl-ln-ch-cl))
             (now (ly:context-current-moment ctx))
             (export-props (ly:context-property ctx 'navigationExportProperties))
             (extra-data (filter-map (cute ly:context-property ctx <> #f)
                                     export-props))
             (ev-length (ly:event-property event 'length ZERO-MOMENT))
             ;; rhythmic-events within grace music have non-grace length, so if
             ;; now has a grace component, convert ev-length to a pure grace
             ;; moment first, then calculate end-moment
             (end (if (= 0 (ly:moment-grace now))
                      (ly:moment-add now ev-length)
                      (ly:moment-sub
                       now (ly:make-moment 0 (- (ly:moment-main ev-length))))))
             (score-ctx (ly:context-find ctx 'Score))
             (nav-table (ly:context-property score-ctx 'navigationTable))
             (file-table (assoc-get filename nav-table '())))
        ;; alist assignment is a convenient way to pre-sort events by file,
        ;; preserving score order when cycling through music expressions
        (ly:context-set-property! score-ctx 'navigationTable
                                  (assoc-set! nav-table filename
                                              (cons (cons* fl-ln-ch-cl
                                                           now end extra-data)
                                                    file-table)))))))

(define-public (record-origins-where-timing! ctx)
  "Add @code{record-origins-listener} to @var{ctx}, if @var{ctx} has a
@code{Timing_translator}; otherwise return @code{#f}."
  (and (eq? ctx (ly:context-find ctx 'Timing))
       (ly:add-listener (record-origins-listener ctx)
                        (ly:context-events-below ctx)
                        'rhythmic-event)))

(define-public (Record_locations_translator ctx)
  (make-translator
   ((initialize translator)
    ;; Since we might want to export metric info as part of nav data, we should
    ;; record events in the same context where Timing_translator lives.
    ;; To avoid needing multiple translators and manual user config, instead of
    ;; writing the listener normally as part of the translator def, we search
    ;; recursively for Timing contexts and explicitly add the listener there.
    (selective-map-contexts record-origins-where-timing! ctx))

   ((finalize translator)
    (let ((nav-table (ly:context-property ctx 'navigationTable)))
      (when (pair? nav-table)
        ;; This is an atypical translator because it reads and writes book-level
        ;; state, and, if it is the last score in the book, writes to disk.
        ;; Its behavior (incrementally recording data per-event, applying both
        ;; score and book post-processing, then writing to output) arguably fits
        ;; the template of a Score translator-group/music-output/output-def type
        ;; similar to \midi{} or \layout{}. The principal advantage to this
        ;; implementation is that it can be attached to $defaultlayout and hence
        ;; work with zero-modification to user code, with existing LilyPond
        ;; releases, simply by including this code via -dinclude-settings,
        ;; Enabling it to be deployed by editing software without waiting for
        ;; users to upgrade to a future LilyPond version that may upstream this.
        (finalize-nav-data! nav-table))))))

(ly:register-translator
 Record_locations_translator 'Record_locations_translator
 '((grobs-created . ())
   (events-accepted . (rhythmic-event))
   (properties-read . (navigationTable
                       navigationExportProperties))
   (properties-written . (navigationTable))
   (description . "\
Collect the start moment, end moment, input location, and value of each property
in @code{navigationExportProperties} for every @code{rhythmic-event}. Organize
this data into a two tables: one collated by filename, the other split into
discrete sequential music expressions. Add the organized data to the book-level
parameters @code{collated-nav-tables} and @code{score-id-alist} respectively.
If this @code{Score} is the last one in its top-level book, write the collected
data from both paramaters to a file named @file{.nav/@var{bookname}.l}, located
relative to the file being compiled.")))

(define-public (count-book-scores book)
  "Recursively count the number of scores in @var{book}."
  (apply + (length (ly:book-scores book))
         (map count-book-scores (ly:book-book-parts book))))

(define (default-toplevel-book-handler book)
  (parameterize ((score-id-alist (list))
                 (collated-nav-tables (list))
                 (book-score-count (count-book-scores book))
                 (current-export-type-conversions
                  (paper-variable book 'export-type-conversions)))
    (toplevel-book-handler book)
    ;; If there's still un-exported nav-data, export it now
    (when (pair? (score-id-alist))
      (export-nav-data))))
