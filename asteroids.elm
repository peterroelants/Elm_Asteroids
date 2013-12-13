-------------------------------------------------
-- Used functions documentation
-------------------------------------------------

-- above : Element -> Element -> Element
-- Stack elements vertically.

-- rect : Float -> Float -> Shape
-- A rectangle with a given width and height.

-- collage : Int -> Int -> [Form] -> Element
-- A collage is a collection of 2D forms.

-- move : (Float,Float) -> Form -> Form
-- Move a form by the given amount.

-- rotate : Float -> Form -> Form
-- Rotate a form by a given angle. Rotate takes standard Elm angles (radians) and turns things counterclockwise. 

-- lift : (a -> b) -> Signal a -> Signal b
-- Transform a signal with a given function.

-- (<~) : (a -> b) -> Signal a -> Signal b
-- An alias for lift (lift f signal == f <~ signal)

-- foldp : (a -> b -> b) -> b -> Signal a -> Signal b
-- Create a past-dependent signal.

-- (.) : (b -> c) -> (a -> b) -> (a -> c)
-- Function composition: (f . g == (\x -> f (g x)))

-- asText : a -> Element
-- Convert anything to it's textual representation and make it displayable in browser

-- map : (a -> b) -> [a] -> [b]
-- Apply a function to every element of a lis

-- foldr : (a -> b -> b) -> b -> [a] -> b
-- Reduce a list from the right: (foldr (+) 0 [1,2,3] == 6)

-- filter : (a -> Bool) -> [a] -> [a]
-- Filter out elements which do not satisfy the predicate: (filter isLower "AaBbCc" == "abc")

-------------------------------------------------
-- Imports
-------------------------------------------------
import Keyboard
import Text
import Random

-------------------------------------------------
-- Types
-------------------------------------------------
-- All objects have a position, velocity, direction and diameter
-- ObjState.x: position on the x-axis from the center
-- ObjState.y: position on the y-axis from the center
-- ObjState.v: velocity in move v*pixelsPerSecond pixels per second
-- ObjState.dir: direction, radians counterclockwise (0=right)
type ObjState a = { a | x:Float, y:Float, v:Float, dir:Float, diam:Float }
-- distinguish between the different objects in the field
type ShipState = ObjState {timeLastBullet:Float}
type AsteroidState = ObjState {}
type BulletState = ObjState {}
type Input = { x:Float, y:Float, space:Bool, delta:Float, rnd:Float }
type FieldState = { ship:ShipState , 
                    asteroids:[ AsteroidState ],
                    bullets:[BulletState] }
data GameState = Lost Float Int | 
                 Won Float Int | 
                 Begin |
                 Game FieldState Int


pixelsPerSecond : Float
pixelsPerSecond = 7

-------------------------------------------------
-- Represenatation
-------------------------------------------------

-- The playing field.
-- There is a playing field of 500x500 pixels, with a black background color.
background : Form
background = rect 500 500 |> filled black

playingField : GameState  -> Element
playingField state = case state of
  Lost time level -> collage 500 500 [rect 500 500 |> filled red, 
                                      endText ("You Lost :(\nlevel: "++(show level))]
  Won time level  -> collage 500 500 [rect 500 500 |> filled green, 
                                      endText ("You Won :)\nlevel: "++(show level))]
  Begin           -> collage 500 500 [rect 500 500 |> filled blue, 
                                      endText "- Ready to play -"]
  Game fieldstate level -> collage 500 500 (listOfForms fieldstate)


endText : String -> Form
endText = 
  toForm . 
    centered . 
      header . 
        (typeface "helvetica") . 
          (Text.color black) . 
            bold  . 
              toText

listOfForms : FieldState -> [Form]
listOfForms state = 
  background :: 
    (showPlane (state.ship.x, state.ship.y) state.ship.dir) :: 
      (map asteroidToForm state.asteroids) ++ 
        (map bulletToForm state.bullets)

asteroidToForm : AsteroidState -> Form
asteroidToForm a = showAsteroid (a.x, a.y) a.diam

bulletToForm : BulletState -> Form
bulletToForm b = showBullet (b.x, b.y)

-- To draw the space ship at a certain position (x,y) and orientation dir, you are required to use the following code. This will draw the space ship as an arrow in that direction.
showPlane : (Float, Float) -> Float -> Form
showPlane pos orientation =
  move pos (rotate orientation  
    (filled white (polygon [(10,0),(-8,-5),(-4,0),(-8,5)])))

showAsteroid : (Float, Float) -> Float -> Form
showAsteroid pos diam = move pos (filled white (ngon 5 diam))

showBullet : (Float, Float) -> Form
showBullet pos = move pos (filled white (square 1))



-------------------------------------------------
-- Signals
-------------------------------------------------

game : Signal Element
game = lift playingField stateSignal

delta : Signal Float
delta = inSeconds <~ fps 20

input : Signal Input
input = sampleOn delta (Input <~ lift (toFloat  . .x) Keyboard.arrows 
                               ~ lift (toFloat . .y) Keyboard.arrows
                               ~ Keyboard.space
                               ~ delta
                               ~ Random.float delta)

stateSignal : Signal GameState 
stateSignal = foldp updateGame  Begin  input

-------------------------------------------------
-- state and updates of state
-------------------------------------------------

-- linear congruential generator 
lcg : Float -> Float
lcg seed = let m = 2147483647
               a = 48271
               x = floor(seed * 2147483647)
           in (toFloat (mod (a*x) m)) / (toFloat m)

--genRandList : Int -> Float -> [Float] -> [Float]
--genRandList len seed (r::rs) =
--  if | len == 0 -> (r::rs)
--     | otherwise -> let rnd = (lcg r)
--                    in (genRandList (len-1) rnd (rnd::r::rs))

startShipState  : ShipState 
startShipState  = {x=0, y=0, v=0, dir=0, diam=8, timeLastBullet=0}

rndStartState : Float -> Int -> GameState
rndStartState seed level =
  let asteroids = rndAsteroids level (3 + (2*level)) seed []
  in Game {ship=startShipState, 
           asteroids=asteroids,
           bullets=[]} level

rndAsteroids : Int -> Int -> Float -> [AsteroidState] -> [AsteroidState] 
rndAsteroids level nb seed ass =
  if | nb == 0 -> ass
     | otherwise -> let (a, rnd1) = rndAsteroid level seed
                    in rndAsteroids level (nb-1) rnd1 (a::ass)

rndAsteroid : Int -> Float -> (AsteroidState, Float)
rndAsteroid level seed = 
  let (pos1, rnd1) = rndPos seed
      (pos2, rnd2) = rndPos rnd1
      (v, rnd3) = rndRange 1 (toFloat (2+level)) rnd2
      (dirDeg, rnd4) = rndRange 0 360 rnd3
      (diam, rnd5) = rndRange 5 40 rnd4
  in ({x=pos1, y=pos2, v=v, dir=(degrees dirDeg), diam=diam}, rnd5)


rndPos : Float -> (Float, Float)
rndPos seed = 
  let (sgn, rnd1) = rndSgn seed
      (pos, rnd2) = (rndRange 75 250 rnd1)
  in ((pos*sgn), rnd2)

rndSgn : Float -> (Float, Float)
rndSgn seed = 
  let rnd = lcg seed
  in if | rnd < 0.5 -> (-1, rnd)
        | otherwise -> (1, rnd)

rndRange : Float -> Float -> Float -> (Float, Float)
rndRange from to seed = 
  let rnd = lcg seed
  in ((from + (rnd * (to - from))), rnd)


-- TODO: weg
startState : GameState
startState = Game {ship=startShipState , 
                   asteroids=[
                   {x=100, y=100, v=4, dir=(degrees -177), diam=10},
                   {x=50, y=50, v=4, dir=(degrees 91), diam=20},
                   {x=-50, y=50, v=4, dir=(degrees 36), diam=20},
                   {x=-50, y=50, v=4, dir=(degrees -348), diam=20},
                   {x=-100, y=-100, v=4, dir=(degrees 198), diam=20}],
                   bullets=[]} 1

updateGame : Input -> GameState -> GameState
updateGame inp state = case state of
  Lost time level -> if | inp.space && time > 1 -> (rndStartState inp.rnd 1)
                        | otherwise -> Lost (time + inp.delta) level
  Won time level ->  if | inp.space && time > 1 -> (rndStartState inp.rnd (level + 1))
                        | otherwise -> Won (time + inp.delta) level
  Begin ->           if | inp.space -> (rndStartState inp.rnd 1)
                        | otherwise -> Begin
  Game fieldstate level -> 
    let newState = updateObjects inp fieldstate
    in if | (shipCollision newState) -> Lost 0 level
          | (isNoAstroidLeft newState) -> Won 0 level
          | otherwise -> Game newState level


isNoAstroidLeft : FieldState -> Bool
isNoAstroidLeft s = (isEmpty s.asteroids)

updateObjects : Input -> FieldState -> FieldState
updateObjects inp state = 
  let (newShip, bullet) = shootBullet inp state.ship
  in bulletCollisions { 
  state | ship <- (shipUpdate inp newShip),
                  asteroids <- (map (\x -> (asteroidUpdate inp x)) (bounceAllAsteroids state.asteroids)),
                  bullets <- cons bullet (updateBullets inp state.bullets) }



shipUpdate : Input -> (ShipState -> ShipState)
shipUpdate inp = 
  bounce . 
    (updatePos inp) . 
      (updateVelocity inp) . 
        (updateDir inp)

shootBullet : Input -> ShipState -> (ShipState, Maybe BulletState)
shootBullet inp s =
  if | inp.space && (s.timeLastBullet > 1) -> 
    let (noseX, noseY) = getNoseOfShip s 
    in ({s | timeLastBullet <- 0}, Just { x=noseX, y=noseY, v=s.v+4, dir=s.dir, diam=1 })
     | otherwise -> ({s | timeLastBullet <- s.timeLastBullet + inp.delta}, Nothing)

getNoseOfShip : ShipState -> (Float, Float)
getNoseOfShip s = (s.x + cos(s.dir) * 10,
                   s.y + sin(s.dir) * 10)


asteroidUpdate : Input -> AsteroidState -> AsteroidState
asteroidUpdate inp state = bounce (updatePos inp state)

bulletUpdate : Input -> BulletState -> BulletState
bulletUpdate inp state = updatePos inp state

updateBullets : Input -> [BulletState] -> [BulletState]
updateBullets inp blist = filter isOutOfField (map (\x -> (bulletUpdate inp x)) blist)

isOutOfField : BulletState -> Bool
isOutOfField state = 
  if | state.x > 250 || state.x < -250 || state.y > 250 || state.y < -250 -> False
     | otherwise -> True

updatePos : Input -> ObjState a -> ObjState a
updatePos inp state = 
    { state | x <- state.x + cos(state.dir) * state.v * inp.delta * pixelsPerSecond , 
              y <- state.y + sin(state.dir) * state.v * inp.delta * pixelsPerSecond }

updateVelocity : Input -> ObjState a -> ObjState a
updateVelocity inp state = { state | v <- state.v + inp.y * inp.delta * 7 }

updateDir : Input -> ObjState a -> ObjState a
updateDir inp state =
    { state | dir <- state.dir - inp.x * inp.delta * 3}

-- If the space ship hits the edge of the playing field, it bounces back. This means that if its orientation was previously dir and it hits a vertical edge (the right or left edge of the field), its new orientation becomes pi - dir. If it hits a horizontal edge (the upper or lower edge), then the new orientation becomes -dir. It must not be possible for the ship to leave the playing field.
bounce : ObjState a -> ObjState a
bounce state = 
    if | state.x > 250  -> bounce {state | dir <- pi - state.dir, 
                                           x <- 250 - (state.x-250)}
       | state.x < -250 -> bounce {state | dir <- pi - state.dir, 
                                           x <- -250 - (state.x+250)}
       | state.y > 250  -> bounce {state | dir <- -state.dir, 
                                           y <- 250 - (state.y-250)}
       | state.y < -250 -> bounce {state | dir <- -state.dir, 
                                           y <- -250 - (state.y+250)}
       | otherwise -> state

bulletCollisions : FieldState -> FieldState
bulletCollisions state = 
  {state | asteroids <- filter (bulletHit state.bullets) state.asteroids,
           bullets <- filter (bulletHit state.asteroids) state.bullets}

bulletHit : [ObjState a] -> ObjState b -> Bool
bulletHit bs a = not (objCollisionList a bs)

shipCollision : FieldState -> Bool
shipCollision state = objCollisionList state.ship state.asteroids

objCollisionList : ObjState a -> [ObjState b] -> Bool
objCollisionList x ys = any (\z -> objCollision x z) ys

objCollision : ObjState a -> ObjState b -> Bool
objCollision s1 s2 = if | (dist s1 s2) < s1.diam + s2.diam -> True
                        | otherwise -> False

dist : ObjState a -> ObjState b -> Float
dist s1 s2 = sqrt( (s1.x-s2.x)^2 + (s1.y-s2.y)^2 )


isEmpty : [a] -> Bool
isEmpty l = (length l) == 0

bounceAllAsteroids : [AsteroidState] -> [AsteroidState]
bounceAllAsteroids a0s = fst (bounceAllAsteroidsWithAllAsteroids [] a0s)

bounceAllAsteroidsWithAllAsteroids : [AsteroidState] -> [AsteroidState] -> ([AsteroidState], [AsteroidState])
bounceAllAsteroidsWithAllAsteroids a0s a1s = 
  case a1s of
    (a1::a1stail) -> let (a1tailnew, a1n, _) =  bounceAsteroidWithAsteroids [] a1 a1stail
                     in bounceAllAsteroidsWithAllAsteroids (a0s++[a1n]) a1tailnew
    otherwise -> (a0s, [])

bounceAsteroidWithAsteroids : [AsteroidState] -> AsteroidState -> [AsteroidState] -> ([AsteroidState], AsteroidState, [AsteroidState])
bounceAsteroidWithAsteroids a0s a1 a2s = 
  case a2s of
    (a2::a2stail) -> let (a1n, a2n) = (checkAsteroidBounce a1 a2)
                     in  bounceAsteroidWithAsteroids (a0s++[a2n]) a1n a2stail
    otherwise -> (a0s, a1, [])

checkAsteroidBounce : AsteroidState -> AsteroidState -> (AsteroidState , AsteroidState)
checkAsteroidBounce a1 a2 = 
  if | (objCollision a1 a2) -> (asteroidBounce a1 a2)
     | otherwise -> (a1, a2)

-- info: http://www.hoomanr.com/Demos/Elastic2/
-- http://en.wikipedia.org/wiki/Elastic_collision
asteroidBounce : AsteroidState -> AsteroidState -> (AsteroidState , AsteroidState)
asteroidBounce a1 a2 = 
  let colA = atan2 (a1.y - a2.y) (a1.x - a2.x) -- collision angle
      iv1x = a1.v * (cos (a1.dir - colA)) -- initial velocity a1 x
      iv1y = a1.v * (sin (a1.dir - colA)) -- initial velocity a1 y
      iv2x = a2.v * (cos (a2.dir - colA)) -- initial velocity a2 x
      iv2y = a2.v * (sin (a2.dir - colA)) -- initial velocity a2 y
      v1 = (sqrt (iv2x^2 + iv1y^2))
      v2 = (sqrt (iv1x^2 + iv2y^2))
      dir1 = (atan2 iv1y iv2x) + colA
      dir2 = (atan2 iv2y iv1x) + colA
  in moveAstroidsApart {a1 | v <- v1, dir <- dir1}
                       {a2 | v <- v2, dir <- dir2}

moveAstroidsApart : AsteroidState -> AsteroidState -> (AsteroidState , AsteroidState)
moveAstroidsApart a1 a2 = 
  if | (objCollision a1 a2) -> moveAstroidsApart { a1 | x <- a1.x + cos(a1.dir) * a1.v * 0.5, y <- a1.y + sin(a1.dir) * a1.v * 0.5} { a2 | x <- a2.x + cos(a2.dir) * a2.v * 0.5, y <- a2.y + sin(a2.dir) * a2.v * 0.5}
     | otherwise -> (a1, a2)

-------------------------------------------------
-- DEBUG
-------------------------------------------------
debugInfo : Signal Input
debugInfo = input -- Whatever info you want to show for debugging

debugEl : Signal Element
debugEl = lift asText debugInfo


-------------------------------------------------
-- MAIN
-------------------------------------------------
main : Signal Element
main = lift2 above game debugEl
