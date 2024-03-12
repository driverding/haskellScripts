import Data.Maybe

wbEins = [
   (1, "ein"),
   (2, "zwei"),
   (3, "drei"),
   (4, "vier"),
   (5, "funf"),
   (6, "sechs"),
   (7, "sieben"),
   (8, "acht"),
   (9, "neun"),
   (10, "zehn"),
   (11, "elf"),
   (12, "zwolf"),
   (13, "dreizehn"),
   (14, "vierzehn"),
   (15, "funfzehn"),
   (16, "sechzehn"),
   (17, "siebzehn"),
   (18, "achtzehn"),
   (19, "neunzehn")
   ]

wbZehn = [
   (2, "zwanzig"),
   (3, "dreissig"),
   (4, "vierzig"),
   (5, "funfzig"),
   (6, "sechzig"),
   (7, "siebzig"),
   (8, "achtzig"),
   (9, "neunzig")
   ]

lkup x y = fromMaybe "ERROR" (lookup x y)

zahlen :: Int -> String
zahlen 0 = "null"
zahlen x = zahlenList (toList x)

toList :: Int -> [Int]
toList 0 = []
toList x = (mod x 1000) : toList (div x 1000)

zahlenList :: [Int] -> String
zahlenList xs = hilfMir 0 xs

hilfMir :: Int -> [Int] -> String
hilfMir l (x:xs) = hilfMir (l+1) xs ++ thisPart ++ morePart
   where
      withOne = ((mod x 10) == 1)

      thisPart = hilfHier l x

      morePart
       | x == 0 = ""
       | l == 0 = ""
       | l == 1 = "tausend"
       | l == 2 = if withOne then " Million " else " Millionen "
       | l == 3 = if withOne then " Milliarde " else " Milliarden "

hilfMir l [] = ""

hilfHier :: Int -> Int -> String
hilfHier l x
 | x == 0           = ""
 | x == 1 && l == 0 = "eins"
 | x == 1 && l >= 2 = "eine"
 | x < 20           = lkup x wbEins
 | x < 100          = lkup (mod x 10) wbEins ++ "und" ++ lkup (div x 10) wbZehn
 | otherwise        = lkup (div x 100) wbEins ++ "hundert" ++ hilfHier l (mod x 100)