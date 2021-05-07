This is a [literate Haskell][] version of a [TTC (Textual Type Classes)][]
example that can be found in [`/examples/prompt`][] on GitHub.  The example
implements a command-line interface for entering credit card details.  This
version of the example is used to test the literate Haskell functionality of
LiterateX.  The [source and `Makefile`][] is also available on GitHub.

The following command can be used to run this example using [Nix][].

```
make run-nix
```

The following command can be used to run this example using [Stack][].

```
make run-stack
```

The following command can be used to load this example in a REPL using
[Nix][].

```
make ghci-nix
```

[literate Haskell]: <https://wiki.haskell.org/Literate_programming>
[TTC (Textual Type Classes)]: <https://github.com/ExtremaIS/ttc-haskell#ttc-textual-type-classes>
[`/examples/prompt`]: <https://github.com/ExtremaIS/ttc-haskell/tree/main/examples/prompt>
[source and `Makefile`]: <https://github.com/ExtremaIS/www.extrema.is-code/tree/main/blog/2021-05-02-literatex-example-literate-haskell>
[Nix]: <https://nixos.org/>
[Stack]: <https://www.haskellstack.org/>

Literate Haskell Notes
----------------------

When writing Markdown in literate Haskell, one must use
[setext-style headings][] because GHC is unable to parse
[ATX-style headings][].

[setext-style headings]: <https://pandoc.org/MANUAL.html#setext-style-headings>
[ATX-style headings]: <https://pandoc.org/MANUAL.html#atx-style-headings>

Module Definition and Imports
-----------------------------

Since this is an example program that is meant to be explored, everything is
exported.

> module Main where
>
> -- https://hackage.haskell.org/package/base
> import Control.Monad (unless, when)
> import Data.Bifunctor (first)
> import Data.Char (digitToInt, isDigit, isSpace, toUpper)
> import Data.List (dropWhileEnd, intersperse)
> import qualified System.IO as IO
> import Text.Read (readMaybe)
>
> -- https://hackage.haskell.org/package/time
> import qualified Data.Time.Calendar as Calendar
> import qualified Data.Time.Clock as Clock
>
> -- https://hackage.haskell.org/package/ttc
> import qualified Data.TTC as TTC

`CreditCard`
------------

The `CreditCard` data structure represents all credit card details, which are
implemented using distinct types below.  Note that a `Show` instance is
derived for debugging purposes only: it allows a `CreditCard` value to be
displayed in the REPL.

> data CreditCard
>   = CreditCard
>     { name           :: !Name
>     , number         :: !Number
>     , expirationDate :: !ExpirationDate
>     , securityCode   :: !SecurityCode
>     }
>   deriving Show

`Name`
------

The `Name` type represents the name on a credit card.  The `Show` instance is
derived for debugging purposes only.

> newtype Name = Name String
>   deriving (Eq, Ord, Show)

The `Parse` instance parses a `Name` from user input.  After any leading and
trailing whitespace is stripped and all lowercase characters are converted to
uppercase, a name must meet the following constraints:

* Only characters between `0x20` (space) and `0x5F` (underscore) are allowed.
* The name must be between 1 and 26 characters in length.

Note that the normalized string is stored in the `Name` data structure.

Reference:

* <https://stackoverflow.com/questions/2004532>

> instance TTC.Parse Name where
>   parse = TTC.asS $ \s -> first TTC.fromS $ do
>     let name' = map toUpper $ strip s
>         invChars = filter ((||) <$> (< ' ') <*> (> '_')) name'
>     unless (null invChars) . Left $
>       "name has invalid character(s): " ++ intersperse ',' invChars
>     when (null name') $ Left "name is empty"
>     when (length name' > 26) $ Left "name has more than 26 characters"
>     pure $ Name name'

The `Render` instance renders a `Name` to display to the user.  It simply
returns a `Textual` representation of the normalized `Name` string.

> instance TTC.Render Name where
>   render (Name name') = TTC.convert name'

`Number`
--------

The `Number` type represents a credit card number.  The `Show` instance is
derived for debugging purposes only.

> newtype Number = Number String
>   deriving (Eq, Ord, Show)

The `Parse` instance parses a `Number` from user input.  After any space and
dash characters are removed, a number must meet the following constraints:

* Only ASCII digits are allowed.
* The number must be between 8 and 19 characters in length.
* The number must have a valid checksum.

Note that the normalized string is stored in the `Number` data structure.

Reference:

* <https://en.wikipedia.org/wiki/Payment_card_number>

> instance TTC.Parse Number where
>   parse = TTC.asS $ \s -> first TTC.fromS $ do
>     let number' = filter ((&&) <$> (/= ' ') <*> (/= '-')) s
>         invChars = filter (not . isDigit) number'
>         len = length number'
>     unless (null invChars) . Left $
>       "number has invalid character(s): " ++ intersperse ',' invChars
>     unless (len >= 8) $ Left "number has fewer than 8 characters"
>     unless (len <= 19) $ Left "number has more than 19 characters"
>     unless (luhn number') $ Left "number checksum is invalid"
>     pure $ Number number'

The `Render` instance renders a `Number` to display to the user.  It simply
returns a `Textual` representation of the normalized `Number` string.

> instance TTC.Render Number where
>   render (Number number') = TTC.convert number'

The `luhn` function implements the Luhn algorithm, which is the checksum used
in credit card numbers.

Reference:

* <https://en.wikipedia.org/wiki/Luhn_algorithm>
* <http://rosettacode.org/wiki/Luhn_test_of_credit_card_numbers#Haskell>

> luhn :: String -> Bool
> luhn
>     = (== 0)
>     . (`mod` 10)
>     . sum
>     . map (uncurry (+) . (`divMod` 10))
>     . zipWith (*) (cycle [1, 2])
>     . map digitToInt
>     . reverse

`ExpirationDate`
----------------

The `ExpirationDate` data type represents the expiration date (year and month)
that is shown on a credit card.  The `Show` instance is derived for debugging
purposes only.

> data ExpirationDate
>   = ExpirationDate
>     { year  :: !Year
>     , month :: !Month
>     }
>   deriving (Eq, Ord, Show)

The `Parse` instance parses an `ExpirationDate` from user input, in `YYYY-MM`
format.  Any leading or trailing whitespace is stripped.

> instance TTC.Parse ExpirationDate where
>   parse = TTC.asS $ \s -> case break (== '-') (strip s) of
>     (year', '-':month') ->
>       ExpirationDate <$> TTC.parse year' <*> TTC.parse month'
>     _ -> Left $ TTC.fromS "expiration date not in YYYY-MM format"

The `Render` instance renders an `ExpirationDate` to display to the user.

> instance TTC.Render ExpirationDate where
>   render (ExpirationDate year' month') =
>     TTC.fromS $ TTC.render year' ++ "-" ++ TTC.render month'

The `toDay` function converts an `ExpirationDate` to a specific day.  A credit
card is assumed to expire on the last day of the specified month.

> toDay
>   :: ExpirationDate
>   -> Calendar.Day
> toDay (ExpirationDate (Year year') (Month month')) =
>     let yearZ = fromIntegral year'
>         day = Calendar.gregorianMonthLength yearZ month'
>     in Calendar.fromGregorian yearZ month' day

`Year`
------

The `Year` type represents the year of an expiration date on a credit card.
The `Show` instance is derived for debugging purposes only.

> newtype Year = Year Int
>   deriving (Eq, Ord, Show)

The `Parse` instance parses a `Year` from user input.  In this implementation,
years from 1900 to 9999 are considered valid.

> instance TTC.Parse Year where
>   parse = TTC.asS $ \s -> first TTC.fromS $ do
>     year' <- maybe (Left "year is not in YYYY format") pure $ readMaybe s
>     unless (year' >= 1900) $ Left "year is before 1900"
>     unless (year' <= 9999) $ Left "year is after 9999"
>     pure $ Year year'

The `Render` instance renders a `Year` to display to the user.

> instance TTC.Render Year where
>   render (Year year') = TTC.convert $ show year'

`Month`
-------

The `Month` type represents the month of an expiration date on a credit card.
The `Show` instance is derived for debugging purposes only.

> newtype Month = Month Int
>   deriving (Eq, Ord, Show)

The `Parse` instance parses a `Month` from user input, where `1` represents
January and `12` represents December.  Months `1` through `9` are parsed even
if they are not zero-padded.

> instance TTC.Parse Month where
>   parse = TTC.asS $ \s -> first TTC.fromS $ do
>     month' <- maybe (Left "month is not in MM format") pure $ readMaybe s
>     unless (month' >= 1 && month' <= 12) $ Left "month is not in 1-12 range"
>     pure $ Month month'

The `Render` instance renders a `Month` to display to the user.

> instance TTC.Render Month where
>   render (Month month')
>     | month' < 10 = TTC.convert $ '0' : show month'
>     | otherwise   = TTC.convert $ show month'

`SecurityCode`
--------------

The `SecurityCode` type represents a credit card security code.  The `Show`
instance is derived for debugging purposes only.

> newtype SecurityCode = SecurityCode String
>   deriving (Eq, Ord, Show)

The `Parse` instance parses a `SecurityCode` from user input.  After any
leading and trailing whitespace is stripped, a security code must meet the
following constraints:

* Only ASCII digits are allowed.
* The number must be 3 or 4 characters in length.

Reference:

* <https://en.wikipedia.org/wiki/Card_security_code>

> instance TTC.Parse SecurityCode where
>   parse = TTC.asS $ \s -> first TTC.fromS $ do
>     let securityCode' = strip s
>         invChars = filter (not . isDigit) securityCode'
>         len = length securityCode'
>     unless (null invChars) . Left $
>       "security code has invalid character(s): " ++ intersperse ',' invChars
>     unless (len >= 3) $ Left "security code has fewer than 3 characters"
>     unless (len <= 4) $ Left "security code has more than 4 characters"
>     pure $ SecurityCode securityCode'

The `Render` instance renders a `SecurityCode` to display to the user.

> instance TTC.Render SecurityCode where
>   render (SecurityCode securityCode') = TTC.convert securityCode'

Helper Functions
----------------

The `strip` function strips any leading or trailing whitespace from a string.

> strip :: String -> String
> strip = dropWhile isSpace . dropWhileEnd isSpace

The `prompt` function displays a prompt and reads a response from the user.
The response is parsed using the passed parse function.  If there is an error,
the error is displayed and the user is prompted again.  Otherwise, the parsed
response is returned.

> prompt
>   :: (String -> Either String a)  -- ^ parse function
>   -> String                       -- ^ prompt string
>   -> IO a
> prompt parse promptString = loop
>   where
>     loop = do
>       putStr promptString
>       IO.hFlush IO.stdout
>       s <- getLine
>       case parse s of
>         Right x -> return x
>         Left err -> do
>           putStrLn $ "  " ++ err
>           loop

The `promptTTC` function implements a prompt for any type that has a `Render`
instance.

> promptTTC
>   :: TTC.Parse a
>   => String  -- ^ prompt string
>   -> IO a
> promptTTC = prompt TTC.parse

The `promptCC` function implements the prompts for entering credit card
information.

> promptCC :: IO CreditCard
> promptCC = CreditCard
>     <$> promptTTC "Enter the name: "
>     <*> promptTTC "Enter the number: "
>     <*>
>       ( ExpirationDate
>           <$> promptTTC "Enter the expiration year (YYYY): "
>           <*> promptTTC "Enter the expiration month (MM): "
>       )
>     <*> promptTTC "Enter the security code: "

Main Function
-------------

The `main` function prompts the user for fake credit card details, displays
the parsed information, and gives information about when the card expires
relative to the current time.

> main :: IO ()
> main = do
>     putStrLn "Please enter some fake credit card details."
>     cc <- promptCC
>     putStrLn $ replicate 78 '-'
>     putStrLn $ "Name:            " ++ TTC.render (name cc)
>     putStrLn $ "Number:          " ++ TTC.render (number cc)
>     putStrLn $ "Expiration date: " ++ TTC.render (expirationDate cc)
>     putStrLn $ "Security code:   " ++ TTC.render (securityCode cc)
>     putStrLn $ replicate 78 '-'
>     today <- Clock.utctDay <$> Clock.getCurrentTime
>     putStrLn . ("This credit card " ++) . (++ "!") $
>       case toDay (expirationDate cc) of
>         expiry
>           | expiry > today -> "expires in " ++ diffDays expiry today
>           | expiry < today -> "expired " ++ diffDays today expiry ++ " ago"
>           | otherwise      -> "expires today"
>   where
>     diffDays :: Calendar.Day -> Calendar.Day -> String
>     diffDays day1 day2 = case Calendar.diffDays day1 day2 of
>       1 -> "1 day"
>       n -> show n ++ " days"
