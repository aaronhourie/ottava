data Accidental = Natural | Flat | Sharp deriving (Eq, Enum)
data NoteName = A | B | C | D | E | F | G deriving (Show, Eq, Enum)
data Note = Note NoteName Accidental deriving (Eq)
data ScaleType = Major | Minor | Blues deriving (Show, Eq, Enum) 
data ScaleName = ScaleName Note ScaleType deriving (Eq)

majorScale :: [Int]
majorScale = [0, 2, 4, 5, 7, 9, 11, 12]
minorScale :: [Int]
minorScale = [0, 2, 3, 5, 7, 8, 10, 12]
bluesScale :: [Int]
bluesScale = [0, 3, 5, 6, 7, 10, 12]

instance Show Accidental where 
    show Sharp = "♯"
    show Natural = "♮"
    show Flat = "♭"

instance Show Note where 
    show (Note x y) = show x ++ show y


raise :: Note -> Note
raise (Note E Natural) = Note F Natural
raise (Note B Natural) = Note C Natural
raise (Note E Sharp) = Note F Sharp
raise (Note B Sharp) = Note C Sharp
raise (Note G Sharp) = Note A Natural
raise (Note name Flat) = Note name Natural
raise (Note name Natural) = Note name Sharp
raise (Note name Sharp) = Note (succ name) Natural

raiseBy :: Int -> Note -> Note
raiseBy n name = (iterate raise name) !! n

lower :: Note -> Note
lower (Note F Natural) = Note E Natural
lower (Note C Natural) = Note B Natural
lower (Note F Flat) = Note E Flat
lower (Note C Flat) = Note B Flat
lower (Note A Flat) = Note G Natural
lower (Note name Sharp) = Note name Natural
lower (Note name Natural) = Note name Flat
lower (Note name Flat) = Note (pred name) Natural

lowerBy :: Int -> Note -> Note
lowerBy n name = (iterate lower name) !! n

-- Builds a scale based on a scale name.
generateScale :: ScaleName -> [Note]
-- Major scales have all sharps or naturals, except F and the flats.
generateScale (ScaleName (Note F Natural) Major) = convertScale Flat (scale majorScale (Note F Natural))
generateScale (ScaleName (Note name Flat) Major) = convertScale Flat (scale majorScale (Note name Flat))
generateScale (ScaleName note Major) = scale majorScale note
-- Minor scales have  all flats or naturals, except E, B, and the Sharps.
generateScale (ScaleName (Note E Natural) Minor) = scale minorScale (Note E Natural)
generateScale (ScaleName (Note B Natural) Minor) = scale minorScale (Note B Natural)
generateScale (ScaleName (Note name Sharp) Minor) = scale minorScale (Note name Sharp)
generateScale (ScaleName note Minor) = convertScale Flat (scale minorScale note)
-- Blues scales are just sharps I guess.
generateScale (ScaleName note Blues) = scale bluesScale note

scale :: [Int] -> Note -> [Note]
scale intervals note = map (\amount -> raiseBy amount note) intervals

convertScale :: Accidental -> [Note] -> [Note]
convertScale _ [] = []
convertScale acc notes = convertNote acc (head notes) : convertScale acc (tail notes)

convertNote :: Accidental -> Note -> Note
convertNote Sharp (Note name Flat) = raise (lower (Note name Flat))
convertNote Flat (Note name Sharp) = lower (raise (Note name Sharp))
convertNote Sharp (Note name Sharp) = Note name Sharp
convertNote Flat (Note name Flat) = Note name Flat 
convertNote _ (Note name Natural) = Note name Natural
