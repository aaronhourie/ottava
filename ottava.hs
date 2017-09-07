-- Enums for terms.
data Accidental = Natural | Flat | Sharp deriving (Eq, Enum)
data NoteName = A | B | C | D | E | F | G deriving (Show, Eq, Enum)
-- Holds a note and an accidental.
data Note = Note NoteName Accidental deriving (Eq)
-- Holds a note and an octave. 
data Tone = Tone Note Int deriving (Eq)
-- Defines a scale.
data ScaleName = ScaleName Tone [Int] deriving (Eq)

-- Scales are defined by intervals from the root note.
-- TODO: These are causing some warnings. Need a better way to define them.
major :: [Int]
major = [0, 2, 4, 5, 7, 9, 11, 12]
minor :: [Int]
minor = [0, 2, 3, 5, 7, 8, 10, 12]
blues :: [Int]
blues = [0, 3, 5, 6, 7, 10, 12]

-- For now, explicitly state naturals.
instance Show Accidental where 
    show Sharp = "♯"
    show Natural = "♮"
    show Flat = "♭"

-- Prints prettier.
instance Show Note where 
    show (Note name acc) = show name ++ show acc

instance Show Tone where
    show (Tone note octave) = show note ++ show octave


-- Raises a note by a semitone. 
-- Will only generate naturals and sharps.
raise :: Tone -> Tone  
raise (Tone (Note E Natural) octave) = Tone (Note F Natural) octave
raise (Tone (Note B Natural) octave)  = Tone (Note C Natural) octave
raise (Tone (Note E Sharp) octave) = Tone (Note F Sharp) octave
raise (Tone (Note B Sharp) octave) = Tone (Note C Sharp) octave
raise (Tone (Note G Sharp) octave) = Tone (Note A Natural) (octave + 1)
raise (Tone (Note name Flat) octave) = Tone (Note name Natural) octave
raise (Tone (Note name Natural) octave) = Tone (Note name Sharp) octave
raise (Tone (Note name Sharp) octave) = Tone (Note (succ name) Natural) octave

-- Raise a note by n semitones.
raiseBy :: Int -> Tone -> Tone
raiseBy n tone= (iterate raise tone) !! n

-- Lowers a note by a semitone. 
-- Will only generate naturals and flats.
lower :: Tone -> Tone
lower (Tone (Note F Natural) octave) = Tone (Note E Natural) octave
lower (Tone (Note C Natural) octave) = Tone (Note B Natural) octave
lower (Tone (Note F Flat) octave) = Tone (Note E Flat) octave
lower (Tone (Note C Flat) octave) = Tone (Note B Flat) octave
lower (Tone (Note A Flat) octave) = Tone (Note G Natural) (octave - 1)
lower (Tone (Note name Sharp) octave) = Tone (Note name Natural) octave
lower (Tone (Note name Natural) octave) = Tone (Note name Flat) octave
lower (Tone (Note name Flat) octave) = Tone (Note (pred name) Natural) octave

-- Lower a note by n semitones.
lowerBy :: Int -> Tone -> Tone 
lowerBy n tone = (iterate lower tone) !! n


-- Builds a scale based on a scale name.
-- TODO: This is getting bloated. Needs refactoring.
generateScale :: ScaleName -> [Tone]
-- Major scales have all sharps or naturals, except F and the flats.
generateScale (ScaleName (Tone (Note F Natural) octave) major) = 
    convertScale Flat (scale major (Tone (Note F Natural) octave))
generateScale (ScaleName (Tone (Note name Flat) octave)  major) =
    convertScale Flat (scale major (Tone (Note name Flat) octave))
generateScale (ScaleName root major) = scale major root
-- Minor scales have  all flats or naturals, except E, B, and the Sharps.
generateScale (ScaleName (Tone (Note E Natural) octave) minor) =
    scale minor (Tone (Note E Natural) octave)
generateScale (ScaleName (Tone (Note B Natural) octave)  minor) =
    scale minor (Tone (Note B Natural) octave)
generateScale (ScaleName (Tone (Note name Sharp) octave) minor) =
    scale minor (Tone (Note name Sharp) octave)
generateScale (ScaleName root minor) = convertScale Flat (scale minor root)
-- Any other scales will just be assumed sharp. 
generateScale (ScaleName root intervals) = scale intervals root

-- Generates a scale based on a set of intervals.
-- The generated scale will always be in terms of sharps. 
scale :: [Int] -> Tone -> [Tone]
scale intervals tone = map (\amount -> raiseBy amount tone) intervals

-- Converts a note to its equivalent accidental.
convertNote :: Accidental -> Tone -> Tone 
convertNote Sharp (Tone (Note name Flat) octave) =
    raise (lower (Tone (Note name Flat) octave))
convertNote Flat (Tone (Note name Sharp) octave) = 
    lower (raise (Tone (Note name Sharp) octave))
convertNote Sharp (Tone (Note name Sharp) octave) =
    Tone (Note name Sharp) octave
convertNote Flat (Tone (Note name Flat) octave) =
    Tone (Note name Flat) octave
convertNote _ (Tone (Note name Natural) octave) = 
    Tone (Note name Natural) octave
-- no double sharps yet, so the note remains the same.
convertNote Natural tone = tone

-- Converts a whole scale into an accidental. 
convertScale :: Accidental -> [Tone] -> [Tone]
convertScale _ [] = []
convertScale acc (head:tail) = convertNote acc head : convertScale acc tail

