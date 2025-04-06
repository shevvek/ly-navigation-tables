# LilyPond Navigation Tables

This library implements backend code for GNU LilyPond to export a mapping between rhythmic events and input file locations. It is intended for use by editing software, such as Frescobaldi and Emacs, to enable features such as "vertical" navigation through the code for a musical score, cycling between the same rhythmic position in different parts. This addresses a longstanding  workflow limitation of LilyPond: the difficulty of quickly editing a specific musical passage across all parts in a large score, especially if it involves inserting or removing measures.

This is a short video demo of vertical navigation through a large LilyPond project using [lilypond-ts-mode for Emacs](https://github.com/shevvek/lilypond-ts-mode):

[![Demo of vertical navigation in Emacs lilypond-ts-mode](https://img.youtube.com/vi/IFtCpMpOM1o/0.jpg)](https://www.youtube.com/watch?v=IFtCpMpOM1o)


## Implementation Guide

### Overview

Editors should provide both a default "score compile" command that injects `-dinclude-settings="navigation.ily"` when invoking `lilypond`, and a "parts compile" command that does not. Editors that distribute `navigation.ily` should also document how users may explicitly include `navigation.ily`, in order to support user projects with their own build infrastructure, such as `make`.

When a LilyPond project is compiled with `navigation.ily`, navigation metadata will be exported to one or more `.l` files in `.nav/`, located in the same directory as the file being compiled. No modification of user code is required. Multi-file project structures, polyrhythmic scores, and custom contexts should all "just work."

Editors should watch `.nav/` subdirectories relative to each open LilyPond file, and load navigation data from new or changed `.l` files. Navigation data can also be loaded from pre-existing `.l` files on opening a LilyPond file. The data format is designed to allow editors to populate rhythmic metadata in a single pass iterating over the text of each input file.

The key idea is that as they iterate through the data and populate per-event metadata, editors can infer boundaries between music expressions wherever adjacent rhythmic events are not strictly sequential, populating metadata describing those boundaries. Then, to navigate "vertically," the editor searches the text to find the next/previous boundary, and then, if that segment belongs to the same `score-id`, searches to find the event at the right rhythmic position. By using textual metadata rather than a lookup table, navigation lookups continue to work even after edits to the text have changed absolute line and column numbers. You can find a reference implementation of this in [lilypond-ts-mode for Emacs](https://github.com/shevvek/lilypond-ts-mode).

### Design assumptions

Vertical navigation relies on the assumption that every input location maps onto only one rhythmic position within only one score.

If the same musical input is included at different times within the same score, e.g. `<< \music { s1 \music } >>`, it is indeterminate which of the duplicate events will be retained. This will affect projects that construct music by quoting or referencing variables for repeated thematic material.

If the same musical input is included in multiple different scores, the most recently compiled score will overwrite navigation metadata. For example, compiling just the flute part would generate navigation data that only includes the flute music, overwriting navigation data from the full score. This is the reason for the above recommendation that editors provide both a "score compile" command that generates navigation data and a "parts compile" command that does not.

(Technically, it could be possible for an editor to implement a smarter method of handling navigation metadata from multiple scores for the same input location. The Emacs reference implementation does not currently do this. It seems challenging and probably not worth it.)

`navigation.scm` uses a custom `default-toplevel-book-handler` to perform export of navigation data. Users with code that also defines `default-toplevel-book-handler` will need to do some Scheme work to enable compatibility.

### File format

Each nav file contains a single generic Scheme/LISP-compatible alist sexp (hence the `.l` extension), with top-level keys `by-input-file` and `by-score`.

`by-score` is an alist with elements of form `(score-id . input-file-list)`. This is meant to be used by editors to allow for vertical navigation across multiple input files, so that when the last expression belong to a given `score-id` is reached in one input file, the navigation lookup can skip to the next file in the list. `score-ids` are sufficiently unique to distinguish between multiple projects that reuse the same book output name.

`by-input-file` is an alist where each key is an input filename, and each value is a list of rhythmic event data from all scores in the same book, sorted by input location within a single input file. Each rhythmic event's data is of form:
```
((line char col) score-id beg-moment end-moment . export-props)
```

By default, moments are stored as floating point numbers representing non-grace timing. Navigation data for grace notes will appear in the correct order but will have the same moment metadata as the adjacent non-grace note.

### Customization

As you'll see from `navigation.ily`, all the magic happens thanks to `Record_locations_translator`.

Users can control navigation metadata generation per-score by adding `\remove Record_locations_translator` or `\consists Record_locations_translator` to their `\midi{}` or `\layout{}` block. `Record_locations_translator` should always belong `Score`. This can be used, for example, to enable navigation metadata generation for a midi-only project, or to disable it for scores other than a project's full score. Note, however, that this will cause compilation warnings if users invoke `lilypond` without including `navigation.ily`. Editors that **do not** provide a "compile parts" command **should** clearly communicate this API to users. Editors that **do** provide a "compile parts" command (as recommended above) may want to provide a wrapper interface to this feature.

You may have noticed from the file format specification above that navigation metadata includes `export-props`. This extra metadata for each event includes the values of each context property named in `Score.navigationExportProperties`. By default, `navigation.ily` includes `measurePosition` and `currentBarNumber`, which could be used for example to implement indentation of mid-measure line breaks. Even though `Record_locations_translator` lives within `Score`, the actual recording of events happens within `Timing`, so tempo and meter information will always be available to record. The intent is that different editing programs may customize `navigationExportProperties` in order to support the development of new editor features.

The type conversions specified in the paper variable `export-type-conversions` are applied recursively to start moment, end moment, and export properties. Editors can customize these conversion rules to make navigation data simpler to import, for instance by converting Scheme `#t`/`#f` to symbols readable as boolean values in the editor language. The type conversion rules would also be a useful hook for an editor that wished to implement grace timing math. `export-type-conversions` is an alist where each entry takes the form `(predicate . conversion)` or `(predicate . #f)`. Rules of the latter form will leave the type unchanged.
