\version "2.24.0"

#(load "navigation.scm")

\paper {
  export-type-conversions =
    #`((,integer? . #f)
       (,vector? . ,vector->list)
       ;; Discard grace timing so editors don't have to worry about moment math.
       (,ly:moment? . ,ly:moment-main)
       ;; Most other languages don't support exact rationals.
       (,exact-rational? . ,exact->inexact))
}

\layout {
  \context {
    \Score
    \consists Record_locations_translator
    navigationExportProperties = #'(measurePosition
                                    currentBarNumber)
  }
}
