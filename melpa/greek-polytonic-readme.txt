This package provides a method for inputting polytonic Greek, based
on the Mac and (to a lesser extent) Windows polytonic Greek
keyboards, with several additional bindings to aid classical
scholars (TODO).
cf. http://unicode.org/charts/PDF/U0370.pdf Greek and Coptic
and http://unicode.org/charts/PDF/U1F00.pdf Greek Extended

Where Unicode specifies a NFC for characters in the above tables,
we use the normalized forms instead.

Where a decomposed sequence of letters and combining diacritics is
necessary, we use the following sequence, modeling the canonical
Unicode decomposition of procomposed characters where available,
and from closest to furthest where unavailable:

<letter>-<length>-<diaeresis>-<breathing>-<accent>-<ypogegrammeni>


For convenience, we list here all the combining diacritics that
this input method inputs, or that a precomposed character that this
input method inputs decomposes into.

<length>:
U+0304 COMBINING MACRON
U+0306 COMBINING BREVE

<diaeresis>:
U+0308 COMBINING DIAERESIS

<breathing>:
U+0313 COMBINING COMMA ABOVE
U+0314 COMBINING REVERSED COMMA ABOVE
U+0308 COMBINING DIAERESIS

<accent>:
U+0301 COMBINING ACUTE ACCENT
U+0300 COMBINING GRAVE ACCENT
U+0342 COMBINING GREEK PERISPOMENI

<ypogegrammeni>:
U+0345 COMBINING GREEK YPOGEGRAMMENI

Relevant known naming defects in Unicode names in the two blocks:
U+039B is GREEK CAPITAL LETTER LAMDA
where GREEK CAPITAL LETTER LAMBDA might be expected.
U+03BB is GREEK SMALL LETTER LAMDA
where GREEK SMALL LETTER LAMBDA might be expected.
