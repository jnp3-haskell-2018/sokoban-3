{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
main = runActivity (withStartScreen (resettable walk5))

data Activity world = Activity
  world
  (Event -> world -> world)
  (world -> Picture)

runActivity :: Activity s -> IO()
runActivity (Activity s r d) = activityOf s r d 

-- Uznałem że gra nie powinna przyjmować zdarzen juz po nacisnieciu --
-- Escape, ale restartowac się dopiero po puszczeniu                --
resettable :: Activity s -> Activity (s, Bool)
resettable (Activity initial reducer renderer) = 
  let resetReducer ev (s, restarting) = case ev of {
    (KeyRelease "Esc") -> (initial, False);
    (KeyPress "Esc") -> (s, True);
    otherwise -> if restarting
      then (s, True)
      else ((reducer ev s), False)
  } in
  let drawer (s, r) = renderer s in
  Activity (initial, False) resetReducer drawer

data SSState world = StartScreen | Running world

startScreen :: Picture
startScreen = scaled 3 3 (lettering "Sokoban!")

withStartScreen :: Activity s -> Activity (SSState s)
withStartScreen (Activity state0 handle draw)
  = Activity state0' handle' draw'
  where
    state0' = StartScreen

    handle' (KeyPress key) StartScreen
         | key == " "                  = Running state0
    handle' _              StartScreen = StartScreen
    handle' e              (Running s) = Running (handle e s)

    draw' StartScreen = startScreen
    draw' (Running s) = draw s

-----------
-- Logic --
-----------

-- Data Definitions --
data Coord = C Integer Integer
(=~=) :: Coord -> Coord -> Bool
(=~=) (C x1 y1) (C x2 y2) = (x1 == x2) && (y1 == y2)

data Direction = R | U | L | D

data Tile = Wall | Ground | Storage | Box | Blank

-- Jak chciałem zrobic
-- data Entity = PlayerEntity | BoxEntity
-- data PlayerEntity = PE {
--   entPos :: Coord,
--   ...
-- }
-- data BoxEntity = BE {
--   entPos :: Coord,
-- }
-- To nie pozwalało mi używać tej samej nazwy (entPos) na pozycje,
-- co niweluje abstrakcje, więc na razie jest tak
data EntityType = PlayerEntityType | BoxEntityType
data Entity = Ent {
  entPos  :: Coord,
  entDir  :: Direction,
  entType :: EntityType
}

type Level = Coord -> Tile

data State = S {
  stPlayer :: Entity,
  stBoxes  :: [Entity],
  stMap    :: Level
}

-- Tile properties definitions --
walkableTile :: Tile -> Bool
walkableTile t = case t of
  Ground -> True
  Storage -> True
  otherwise -> False

-- Initial state --
initialCoord :: Coord
initialCoord = C 0 0

initialState = 
  let (boxes, noBoxLevel) = removeBoxes myLevel in
  S {
    stPlayer = Ent {
      entPos = initialCoord,
      entDir = U,
      entType = PlayerEntityType
    },
    stBoxes = boxes,
    stMap = noBoxLevel
  }

-- Directional movement --
adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord d (C x y) = case d of
  R -> (C (x + 1) y)
  U -> (C x (y + 1))
  L -> (C (x - 1) y)
  D -> (C x (y - 1))

moveCoords :: [Direction] -> Coord -> Coord
moveCoords dirs start = foldl (\a -> \b -> adjacentCoord b a) start dirs

moveFromEvent :: Event -> Maybe Direction
moveFromEvent (KeyPress key)
    | key == "Right" = Just R
    | key == "Up"    = Just U
    | key == "Left"  = Just L
    | key == "Down"  = Just D
moveFromEvent _      = Nothing


-- Selectors --
selectEntities :: State -> [Entity]
selectEntities s = (stPlayer s:stBoxes s)

selectIsTileFree :: State -> Coord -> Bool
selectIsTileFree s c = 
  let noColisionWithEntity = all (\e -> not (entPos e =~= c)) (selectEntities s) in
  let noColisionWithTile   = walkableTile (stMap s c) in
  noColisionWithEntity && noColisionWithTile

selectIsWinning :: State -> Bool
selectIsWinning s = all 
  (\b -> case (stMap s) (entPos b) of {
    Storage -> True;
    otherwise ->False
  })
  (stBoxes s)
-- Żeby była funkcja o zadanej nazwie, jednak w kodzie chciałem
-- trzymać jakieś konwencje nazwenicze
isWinning = selectIsWinning

-- Entity constricted movement --
pushEntity :: State -> Entity -> Direction -> Entity
pushEntity s ent dir = 
  let nextPos = adjacentCoord dir (entPos ent) in
  if selectIsTileFree s nextPos 
    then ent {entPos = nextPos, entDir = dir}
    else ent {entDir = dir}

-- Player interaction --
playerMove :: Direction -> State -> State
playerMove d s = 
  let affectBox box = if (entPos box) =~= (adjacentCoord d (entPos (stPlayer s)))
      then (pushEntity s box d)
      else box
  in
  let stateWPushedBox = s {stBoxes = map affectBox (stBoxes s)} in
  stateWPushedBox {stPlayer = (pushEntity stateWPushedBox (stPlayer stateWPushedBox) d)} 

-- Game core --
type GameReducer = Event -> State -> State
type EventConsumer = Event -> State -> (State, Bool)

winnerConsumer :: EventConsumer
winnerConsumer ev s = (s, selectIsWinning s) 

movementConsumer :: EventConsumer
movementConsumer ev s =
  let dir = moveFromEvent ev in
  case dir of
    Just d -> (playerMove d s, True)
    otherwise -> (s, False)

composeConsumers :: [EventConsumer] -> GameReducer 
composeConsumers consumers ev s =
  let process (state, consumed) consumer = if consumed 
      then (state, True)
      else consumer ev state
  in
  let (out, _) = foldl process (s, False) consumers in out

handleEvent = composeConsumers [
  winnerConsumer,
  movementConsumer
  ]

walk5 :: Activity State
walk5 = (Activity initialState handleEvent (renderer mapRenderer entityRenderer))

-------------
-- Drawing --
-------------

-- Tile drawer --
drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = wrong -- Box should not be drawn as a tile 
drawTile Blank   = wrong

-- Textures --
simpleSquare = solidRectangle 1 1

wall = colored (dark gray) simpleSquare

ground = colored gray simpleSquare

storage = 
  (colored red (thickCircle 0.1 0.4)) 
  & (colored gray simpleSquare)

box = 
  (colored (dark brown) (thickRectangle 0.1 0.9 0.9)) 
  & (rotated (pi / 4) (colored (dark brown) (solidRectangle 0.1 1.15)))
  & (rotated (pi / (-4)) (colored (dark brown) (solidRectangle 0.1 1.15)))
  & (colored brown simpleSquare)

wrong = colored pink simpleSquare

triangle = solidPolygon [((-0.5), 0.5), ((-0.5), (-0.5)), (0.5, 0)]

player1 = 
  (colored (yellow) triangle)
  
rotateTowards :: Direction -> Picture -> Picture
rotateTowards d p = case d of
  R -> p
  U -> rotated (pi / 2) p
  L -> rotated pi p
  D -> rotated (pi / (-2)) p
  
-- Core render
type Visibility = [Coord]

drawAt :: Coord -> Level -> Picture
drawAt (C px py) l = 
  let visible = [(C (x + px) (y + py)) | x <- [-10 .. 10], y <- [-10 .. 10]] in
  translated (fromIntegral (-px)) (fromIntegral (-py)) (drawVisible visible l)


drawVisible :: Visibility -> Level -> Picture
drawVisible vis level = 
  foldl 
    (\p1 -> \p2 -> p1 & p2)
    blank
    [translated (fromIntegral x) (fromIntegral y) (drawTile (level (C x y))) | (C x y) <- vis]

type GameRenderer = State -> Picture
type EntityRenderer = State -> Picture
type MapRenderer = State -> Picture


renderer :: MapRenderer -> EntityRenderer -> GameRenderer
renderer mr pr s = 
  let winnerOverlay = if selectIsWinning s 
      then scaled 3 3 (lettering "You won!")
      else blank
  in winnerOverlay & (pr s) & (mr s)

mapRenderer :: MapRenderer
mapRenderer s = (drawAt initialCoord (stMap s))
  
  
drawEntity :: Entity -> Picture
drawEntity ent =
  let entityPicture = case entType(ent) of {
    PlayerEntityType -> player1;
    BoxEntityType -> box
  } in
  let (C x y) = entPos(ent) in
  translated (fromIntegral x) (fromIntegral y) (rotateTowards (entDir ent) entityPicture)


entityRenderer :: EntityRenderer
entityRenderer s = foldl (&) blank [drawEntity(ent) | ent <- selectEntities(s)]

  
--------------------
-- Level Composer --
--------------------

-- Box Remover --
removeBoxes :: Level -> ([Entity], Level)
removeBoxes level =
  let 
    boxRemover (b, l) coord = 
      let tile = l coord 
      in case tile of {
        Box -> ((Ent {entPos = coord, entDir = U, entType = BoxEntityType}:b), \c -> if c =~= coord then Ground else l c);
        otherwise -> (b, l);
      }
  in
    foldl 
      boxRemover
      ([], level)
      [(C x y) | x <- [-20 .. 20], y <- [-20 .. 20]]
      
-- Box Adder --
addBoxes :: ([Entity], Level) -> Level
addBoxes (boxes, level) (C x y) = 
  let boxChecker Ent {entPos = (C bx by)} = (x == bx) && (y == bx) in
  if (any boxChecker boxes) then Box else level (C x y)

-- Composer core --
type LevelFragment = Coord -> Maybe Tile

composeFragments :: [LevelFragment] -> LevelFragment
composeFragments [] = \(C x y) -> Nothing
composeFragments (h:t) = \(C x y) -> 
  let tile = h (C x y) in
  case tile of 
    Nothing -> composeFragments t (C x y)
    Just n -> Just n

asLevel :: LevelFragment -> Level
asLevel frag = \(C x y) -> 
  let tile = frag (C x y) in
  case tile of
    Nothing -> Blank
    Just n -> n

-- Level Fragment translations --
mirrorX :: LevelFragment -> LevelFragment
mirrorX frag = \(C x y) -> frag (C (-x) y)

mirrorY :: LevelFragment -> LevelFragment
mirrorY frag = \(C x y) -> frag (C x (-y))

rotate90 :: LevelFragment -> LevelFragment
rotate90 frag = \(C x y) ->frag (C (-y) x)

rotate180 :: LevelFragment -> LevelFragment
rotate180 frag = rotate90 (rotate90 frag)

rotate270 :: LevelFragment -> LevelFragment
rotate270 frag = rotate90 (rotate180 frag)

translateFragment :: Integer -> Integer -> LevelFragment -> LevelFragment
translateFragment dx dy frag = \(C x y) -> frag (C (x - dx) (y - dy)) 

-- Base Level Fragments --
tileFill :: Tile -> Coord -> Coord -> LevelFragment
tileFill tile (C sX sY) (C eX eY) = 
  \(C x y) -> if (x >= sX) && (x <= eX) && (y >= sY) && (y <= eY) then Just tile else Nothing

hLine :: Tile -> Integer -> Integer -> Integer -> LevelFragment
hLine tileId wy xs xe = tileFill tileId (C xs wy) (C xe wy)
hWall = hLine Wall

vLine :: Tile -> Integer -> Integer -> Integer -> LevelFragment
vLine tileId wx ys ye = tileFill tileId (C wx ys) (C wx ye)
vWall = vLine Wall

tileAt :: Tile -> Coord -> LevelFragment
tileAt tileId (C tx ty) = 
  \(C x y) -> if (x == tx) && (y == ty) then Just tileId else Nothing

wallAt = tileAt Wall
groundAt = tileAt Ground
boxAt = tileAt Box
storageAt = tileAt Storage

-- Level Composer test level --
myFragment :: LevelFragment
myFragment = composeFragments [
  boxAt (C 0 1),
  storageAt (C 1 2),
  hWall (-3) (-3) 3,
  hWall 3 (-3) 3,
  vWall (-3) (-3) 3,
  vWall 3 (-3) 3,
  tileFill Ground (C (-3) (-3)) (C 3 3)
  ]
  
myLevel = asLevel (
  composeFragments [
    tileAt Ground (C 3 0),
    mirrorX myFragment,
    (translateFragment 6 0 (rotate90 myFragment))
    ]
  )
  
----------
-- Maze --
----------
maze :: Level
maze (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground
  
pictureOfMaze = drawAt (C 0 0) maze
