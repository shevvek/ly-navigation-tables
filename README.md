# LilyPond Navigation Tables

This library implements backend code for GNU LilyPond to export a mapping between rhythmic events and input file locations. It is intended for use by editing software, such as Frescobaldi and Emacs, to enable features such as "vertical" navigation through the code for a musical score, cycling between the same rhythmic position in different parts.

## Implementation Guide

### Overview

Editors should provide both a default "score compile" command that injects `-dinclude-settings="navigation.ily"` when invoking `lilypond`, and a "parts compile" command that does not. Editors that distribute `navigation.ily` should also document how users may explicitly include `navigation.ily`, in order to support user projects with their own build infrastructure, such as `make`.

When a LilyPond project is compiled with `navigation.ily`, navigation metadata will be exported to one or more `.l` files in `.nav/`, located in the same directory as the file being compiled. No modification of user code is required. Multi-file project structures, polyrhythmic scores, and custom contexts should all "just work."

Editors should watch `.nav/` subdirectories relative to each open LilyPond file, and load navigation data from new or changed `.l` files. Navigation data can also be loaded from pre-existing `.l` files on opening a LilyPond file.

The data format is intended to make it simple for editors to implement "vertical" navigation by providing both a collated list of rhythmic events sorted by input file location, suitable for quickly iterating over open files and attaching metadata to the text, and a table of sequential music expressions belonging to each score, suitable for use as a global lookup table.

The basic strategy for implementing vertical navigation is to use rhythmic metadata from the current code position to perform a relative lookup in the global navigation table, then go to the resulting file location. You can find a reference implementation of this in [lilypond-ts-mode for Emacs](https://github.com/shevvek/lilypond-ts-mode).

### Design assumptions

Vertical navigation relies on the assumption that every input location maps onto only one rhythmic position within only one score.

If the same musical input is included at different times within the same score, e.g. `<< \music { s1 \music } >>`, it is indeterminate which of the duplicate events will be retained. This will affect projects that construct music by quoting or referencing variables for repeated thematic material.

If the same musical input is included in multiple different scores, the most recently compiled score will overwrite navigation metadata. For example, compiling just the flute part would generate navigation data that only includes the flute music, overwriting navigation data from the full score. This is the reason for the above recommendation that editors provide both a "score compile" command that generates navigation data and a "parts compile" command that does not.

(Technically, it could be possible for an editor to implement a smarter method of handling navigation metadata for the same input location from multiple scores. The Emacs reference implementation does not currently do this. It seems challenging and probably not worth it.)

`navigation.scm` uses a custom `default-toplevel-book-handler` to perform export of navigation data. Users with code that also defines `default-toplevel-book-handler` will need to do some Scheme work to enable compatibility.

### File format

Each nav file contains a single generic Scheme/LISP-compatible alist sexp (hence the `.l` extension), with top-level keys `by-input-file` and `by-score`. Both top-level tables store the *same data*, just organized and formatted differently. For both, the individual data points are lists representing input location, start moment, end moment, and additional metadata for an individual `rhythmic-event` such as a note or rest. The intent is that an editor will keep the `by-score` alist in memory as a lookup table, and use the `by-input-file` data to iterate over the text of open input files, populating metadata.

The format is as follows:

**top-level:**
```
((by-score
 . ((score_id_0 . score_list_0)
    (score_id_1 . score_list_1)
    ...
    (score_id_n . score_list_n)))
(by-input-file
 . ((filename_0 . file_alist_0)
    (filename_1 . file_alist_1)
    ...
    (filename_n . file_alist_n))))
```
**score_list_i:**
```
(segment_0 segment_1 ... segment_n)
```

**seg_j:**
```
(event_n event_n-1 ... event_0)
```

**event_k:**
```
((beg-moment . end-moment) (filename line char col) export-props)
```

**file_alist_i:**
```
((location_0 . data_0)
 (location_1 . data_1)
 ...
 (location_n . data_n))
```

**location_j:**
```
(line char col)
```

**data_j:**
```
(score-id segment-index beg-moment end-moment . export-props)
```

Each alist in `by-input-file` is sorted by ascending input location.

The list associated with each `score-id` in `by-score` is sorted by the order each sequential music segment appears in the code, but the lists of events within each segment are sorted in reverse order. `score-ids` are sufficiently unique for editors to use a single global lookup table even across multiple projects that reuse the same book output name.

For simplicity and read-compatibility across languages, moments are stored as floating point numbers representing non-grace timing. Navigation data for grace notes will appear in the correct order but will have the same moment metadata as the adjacent non-grace note.

### Navigation lookups

This is the basic lookup chain to implement vertical navigation through the code for a score, cycling between the input locations corresponding to a given rhythmic position across all polyphonic parts:
1. `score-id = this-score-id`
2. `segment-id = destination-segment-id`. For cycling forward or backward, `segment-id = this-segment-id +/- 1`. Note that for music with temporary polyphony, not all segments will encompass the same moment span. So destination segments should be filtered to ensure the current moment is in-bounds.
3. `beg-moment <= this-beg-moment < end-moment` For efficiency, this can be done simply via `assoc` (or your language's equivalent of an ordered dictionary lookup) with `<=` as the "equality" test, since the segments are sorted in reverse.

### Customization

As you'll see from `navigation.ily`, all the magic happens thanks to a **Score**-level translator `Record_locations_translator`.

Users can control navigation metadata generation per-score by adding `\remove Record_locations_translator` or `\consists Record_locations_translator` to their `\midi{}` or `\layout{}` block. `Record_locations_translator` should always belong `Score`. This can be used, for example, to enable navigation metadata generation for a midi-only project, or to disable it for scores other than a project's full score. Note, however, that this will cause compilation warnings if users invoke `lilypond` without including `navigation.ily`. Editors that **do not** provide a "compile parts" command **should** clearly communicate this API to users. Editors that **do** provide a "compile parts" command (as recommended above) may want to provide a wrapper interface to this feature.

You may have noticed from the file format specification above that navigation metadata includes `export-props`. This extra metadata for each event includes the values of each context property named in `Score.navigationExportProperties`. By default, `navigation.ily` includes `measurePosition` and `currentBarNumber`, which could be used for example to implement identation of mid-measure line breaks. The intent is that different editing programs may customize `navigationExportProperties` in order to support the development of new editor features. Note that depending on the types of these context properties, they may be exported in a format that requires some parsing when read by languages other than Guile.

Even though `Record_locations_translator` always lives within `Score`, the actual recording of events happens within `Timing`, so tempo and meter information will always be available to record.
