-- Enums for terms.
data Accidental = Natural | Flat | Sharp deriving (Eq, Enum)
data NoteName = A | B | C | D | E | F | G deriving (Show, Eq, Enum)
-- Holds a note and an accidental.
data Note = Note NoteName Accidental deriving (Eq)
-- Defines a scale.
data ScaleName = ScaleName Note [Int] deriving (Eq)

-- Scales are defined by intervals from the root note.
major :: [Int]
major= [0, 2, 4, 5, 7, 9, 11, 12]
minor :: [Int]
minor= [0, 2, 3, 5, 7, 8, 10, 12]
blues :: [Int]
blues= [0, 3, 5, 6, 7, 10, 12]

-- For now, explicitly state naturals.
instance Show Accidental where 
    show Sharp = "♯"
    show Natural = "♮"
    show Flat = "♭"

-- Prints prettier.
instance Show Note where 
    show (Note x y) = show x ++ show y


-- Raises a note by a semitone. 
-- Will only generate naturals and sharps.
raise :: Note -> Note
raise (Note E Natural) = Note F Natural
raise (Note B Natural) = Note C Natural
raise (Note E Sharp) = Note F Sharp
raise (Note B Sharp) = Note C Sharp
raise (Note G Sharp) = Note A Natural
raise (Note name Flat) = Note name Natural
raise (Note name Natural) = Note name Sharp
raise (Note name Sharp) = Note (succ name) Natural

-- Raise a note by n semitones.
raiseBy :: Int -> Note -> Note
raiseBy n name = (iterate raise name) !! n

-- Lowers a note by a semitone. 
-- Will only generate naturals and flats.
lower :: Note -> Note
lower (Note F Natural) = Note E Natural
lower (Note C Natural) = Note B Natural
lower (Note F Flat) = Note E Flat
lower (Note C Flat) = Note B Flat
lower (Note A Flat) = Note G Natural
lower (Note name Sharp) = Note name Natural
lower (Note name Natural) = Note name Flat
lower (Note name Flat) = Note (pred name) Natural

-- Lower a note by n semitones.
lowerBy :: Int -> Note -> Note
lowerBy n name = (iterate lower name) !! n


-- Builds a scale based on a scale name.
-- TODO: This is getting bloated. Needs refactoring.
generateScale :: ScaleName -> [Note]
-- Major scales have all sharps or naturals, except F and the flats.
generateScale (ScaleName (Note F Natural) major) = convertScale Flat (scale major (Note F Natural))
generateScale (ScaleName (Note name Flat) major) = convertScale Flat (scale major (Note name Flat))
generateScale (ScaleName root Major) = scale majorScale root
-- Minor scales have  all flats or naturals, except E, B, and the Sharps.
generateScale (ScaleName (Note E Natural) minor) = scale minor (Note E Natural)
generateScale (ScaleName (Note B Natural) minor) = scale minor (Note B Natural)
generateScale (ScaleName (Note name Sharp) minor) = scale minor (Note name Sharp)
generateScale (ScaleName root Minor) = convertScale Flat (scale minor root)
-- Any other scales will just be assumed sharp. 
generateScale (ScaleName root scaleType) = scale scaleType root

-- Generates a scale based on a set of intervals.
-- The generated scale will always be in terms of sharps. 
scale :: [Int] -> Note -> [Note]
scale intervals note = map (\amount -> raiseBy amount note) intervals


-- Converts a note to its equivalent accidental.
convertNote :: Accidental -> Note -> Note
convertNote Sharp (Note name Flat) = raise (lower (Note name Flat))
convertNote Flat (Note name Sharp) = lower (raise (Note name Sharp))
convertNote Sharp (Note name Sharp) = Note name Sharp
convertNote Flat (Note name Flat) = Note name Flat 
convertNote _ (Note name Natural) = Note name Natural
-- no double sharps yet, so the note remains the same.
convertNote Natural note = note

-- Converts a whole scale into an accidental. 
convertScale :: Accidental -> [Note] -> [Note]
convertScale _ [] = []
convertScale acc (head:tail) = convertNote acc head : convertScale acc tail


