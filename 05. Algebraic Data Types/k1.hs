import Data.List (nub, partition)

data Song
  = Song
  { _title :: String
  , _artist :: String
  , _genre :: String
  , _duration :: Int
  }

-- типов синоним
-- type Playlist = [Song]

data Playlist = Playlist [Song]

duration :: Playlist -> Int
duration (Playlist songs) =
  -- _duration :: Song -> Int
  -- (+) :: Int -> Int -> Int
  -- ((+) . _duration)
  -- foldr (\song acc -> _duration song + acc) 0 songs
  -- foldl (\acc song -> acc + _duration song) 0 songs
  -- sum [_duration x | x <- songs]
  sum $ map _duration songs

shorterThan :: Int -> Playlist -> Playlist
shorterThan dur (Playlist songs) =
  -- Playlist [song | song <- songs, _duration song < dur]
  Playlist $ filter ((< dur) . _duration) songs

artists :: Playlist -> [String]
artists (Playlist songs) = nub $ map _artist songs

-- [("classical", 5),...]

-- x1 == x2

-- x = 1 ~> (1, 3)
-- [1,2,3,1,1,5,5]

hist :: Eq a => [a] -> [(a, Int)]
hist [] = []
hist l@(x : xs) =
  let (eq, neq) = partition (== x) l
   in (x, length eq) : hist neq

song1 :: Song
song1 = Song "hello1" "" "pop" 0

song2 :: Song
song2 = Song "hello2" "" "pop" 0

song3 :: Song
song3 = Song "hello3" "" "jazz" 0

song4 :: Song
song4 = Song "hello4" "" "rock" 0

song5 :: Song
song5 = Song "hello5" "" "jazz" 0

songs :: Playlist
songs = Playlist [song5, song1, song2, song4, song3]

mostPopularGenre :: Playlist -> [String]
mostPopularGenre (Playlist songs) = titles
 where
  genres = map _genre songs
  genreHist = hist genres
  maxGenreOccurrence = maximum $ map snd genreHist

  -- max: 3
  -- [(g1,3), (g2,3), (g3,1), ...]

  mostPopular :: [String]
  mostPopular =
    [ genre
    | (genre, n) <- genreHist
    , n == maxGenreOccurrence
    ]

  songs' = filter (\song -> _genre song `elem` mostPopular) songs
  titles = map _title songs'
