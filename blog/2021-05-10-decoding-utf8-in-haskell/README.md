This code accompanies the
[Decoding UTF-8 in Haskell](https://www.extrema.is/blog/2021/05/10/decoding-utf8-in-haskell)
blog entry.

Data file: [COVID-19 Case Surveillance Public Use Data](https://data.cdc.gov/Case-Surveillance/COVID-19-Case-Surveillance-Public-Use-Data/vbim-akqf) CSV file

One of the benchmark implementations skips blank lines!  The CSV file does not
contain blank lines, but it is an issue when processing other files.
[final version](../2021-05-11-decoding-utf8-in-haskell-part-2)
