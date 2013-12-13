-------------------------------------------------
-- Imports
-------------------------------------------------
import Keyboard
import Text
import Random


-------------------------------------------------
-- Types
-------------------------------------------------
-- All objects have a position, velocity, direction and diameter.
-- ObjState.x: position on the x-axis from the center
-- ObjState.y: position on the y-axis from the center
-- ObjState.v: velocity in move v*pixelsPerSecond pixels per second
-- ObjState.dir: direction, radians counterclockwise (0=right)
-- ObjState.diam: diameter of the object
type ObjState a = { a | x:Float, y:Float, v:Float, dir:Float, diam:Float }

-- Distinguish between the different objects in the field.

-- ShipState is the representation of a ship.
-- ShipState.timeLastBullet: holds the time past since the last bullet was fired
type ShipState = ObjState {timeLastBullet:Float}

-- AsteroidState is the representaion of an asteroid.
type AsteroidState = ObjState {}

-- BulletState is the representation of a bullet.
type BulletState = ObjState {}

-- Input signal.
-- Input.x: left/right input
-- Input.y: up/down input
-- Input.space: true if and only if the space bar is pressed
-- Input.delta: time since last update
-- Input.rnd: random seed
type Input = { x:Float, y:Float, space:Bool, delta:Float, rnd:Float }

-- FieldState is the representation of the playing field.
-- FieldState.ship: The state of the ship in the field
-- FieldState.asteroids: The state of the asteroids in the field
-- FieldState.bullets: The state of the bullets in the field
type FieldState = { ship:ShipState , 
                    asteroids:[ AsteroidState ],
                    bullets:[BulletState] }

-- GameState is the state of the game.
-- GameState = Lost time level: Game is lost 'time' seconds ago at level 'level'
-- GameState = Won time level: Game is won 'time' seconds ago at level 'level'
-- GameState = Begin: Start of game, nothing happend yet
-- GameState = Game field level: Game is in progress with the state of the field as 
--             'fieldstate' and at level 'level'
data GameState = Lost Float Int | 
                 Won Float Int | 
                 Begin |
                 Game FieldState Int


-------------------------------------------------
-- Represenatation
-------------------------------------------------

-- The background of the playing field.
-- There is a playing field of 500x500 pixels, with a black background color.
background : Form
background = rect 500 500 |> filled black

-- Display the Game state.
playingField : GameState  -> Element
playingField state = case state of
  Lost time level -> collage 500 500 [rect 500 500 |> filled red, 
                                      endText black ("You Lost :(\nlevel: "++(show level))]
  Won time level  -> collage 500 500 [rect 500 500 |> filled green, 
                                      endText black ("You Won :)\nlevel: "++(show level))]
  Begin           -> collage 500 500 [rect 500 500 |> filled black, 
                                      endText white "- Ready to play -"]
  Game fieldstate level -> collage 500 500 (listOfForms fieldstate)

-- Display any text in the middle of the field.
endText : Color -> String-> Form
endText col = 
  toForm . 
    centered . 
      header . 
        (typeface "helvetica") . 
          (Text.color col) . 
            bold  . 
              toText

-- Turn the fieldstate in a list of forms to represent on the screen.
listOfForms : FieldState -> [Form]
listOfForms state = 
  background :: 
    (showPlane (state.ship.x, state.ship.y) state.ship.dir) :: 
      (map asteroidToForm state.asteroids) ++ 
        (map bulletToForm state.bullets)

-- Turn the asteroidstate in a form to represent on the screen.
asteroidToForm : AsteroidState -> Form
asteroidToForm a = showAsteroid (a.x, a.y) a.diam

-- Turn the BulletState in a form to represent on the screen.
bulletToForm : BulletState -> Form
bulletToForm b = showBullet (b.x, b.y)

-- Draw a ship at the given position.
showPlane : (Float, Float) -> Float -> Form
showPlane pos orientation =
  move pos (rotate orientation  
    (filled white (polygon [(10,0),(-8,-5),(-4,0),(-8,5)])))

-- Show an asteroid at the given position.
showAsteroid : (Float, Float) -> Float -> Form
showAsteroid pos diam = move pos (filled white (ngon 5 diam))

-- Show a bullet at the given position.
showBullet : (Float, Float) -> Form
showBullet pos = move pos (filled white (square 1))


-------------------------------------------------
-- Signals
-------------------------------------------------

-- General the game signal that is shown.
game : Signal Element
game = lift playingField stateSignal

-- Generate frames.
-- delta returns the time between the last 2 frames.
delta : Signal Float
delta = inSeconds <~ fps 20

-- Generate the input signal on each tick of the frame.
input : Signal Input
input = sampleOn delta (Input <~ lift (toFloat  . .x) Keyboard.arrows 
                               ~ lift (toFloat . .y) Keyboard.arrows
                               ~ Keyboard.space
                               ~ delta
                               ~ Random.float delta)

-- Generate the game state signal that is the game going on.
stateSignal : Signal GameState 
stateSignal = foldp updateGame  Begin  input


-------------------------------------------------
-- Initial state
-------------------------------------------------

-- The initial state of the ship.
startShipState  : ShipState 
startShipState  = {x=0, y=0, v=0, dir=0, diam=8, timeLastBullet=0}

-- The state is randomly generated according to the current level.
-- To generate the state randomly a random seed needs to be given to start with
-- new random numbers will be generated from this random seed with the help of the
-- 'linear congruential generator'. Each generated random number will be used as seed
-- for the next use untill the state is completely generated.

-- Linear congruential generator.
-- Input a random seed and output a new random number.
lcg : Float -> Float
lcg seed = let m = 2147483647
               a = 48271
               x = floor(seed * 2147483647)
           in (toFloat (mod (a*x) m)) / (toFloat m)

-- Generate a random start stating state with the difficulty depending on 'level' 
-- and with 'seed' as the random number seed.
rndStartState : Float -> Int -> GameState
rndStartState seed level =
  let asteroids = rndAsteroids level (3 + (2*level)) seed []
  in Game {ship=startShipState, 
           asteroids=asteroids,
           bullets=[]} level


-- Generate a list of random asteroids
-- rndAsteroids level nb seed ass
--    level: the current level
--    nb: the number of asteroids to be generated
--    seed: the random number seed to start from
--    ass: accumulator list of already generated asteroids
rndAsteroids : Int -> Int -> Float -> [AsteroidState] -> [AsteroidState] 
rndAsteroids level nb seed ass =
  if | nb == 0 -> ass
     | otherwise -> let (a, rnd1) = rndAsteroid level seed
                    in rndAsteroids level (nb-1) rnd1 (a::ass)


-- Generate an asteroid for the curren level with 'seed' the random number seed 
-- to start from.
-- Returns the asteroid and the next random seed.
rndAsteroid : Int -> Float -> (AsteroidState, Float)
rndAsteroid level seed = 
  let (pos1, rnd1) = rndPos seed
      (pos2, rnd2) = rndPos rnd1
      (v, rnd3) = rndRange 1 (toFloat (2+level)) rnd2
      (dirDeg, rnd4) = rndRange 0 360 rnd3
      (diam, rnd5) = rndRange 5 40 rnd4
  in ({x=pos1, y=pos2, v=v, dir=(degrees dirDeg), diam=diam}, rnd5)

-- Generate a random position from the given random seed.
-- Returns the position and the next random seed.
rndPos : Float -> (Float, Float)
rndPos seed = 
  let (sgn, rnd1) = rndSgn seed
      (pos, rnd2) = (rndRange 75 250 rnd1)
  in ((pos*sgn), rnd2)

-- Generate a random sign (+1/-1) from the given seed.
-- Returns the sign and the next random seed.
rndSgn : Float -> (Float, Float)
rndSgn seed = 
  let rnd = lcg seed
  in if | rnd < 0.5 -> (-1, rnd)
        | otherwise -> (1, rnd)

-- Generate a float between the given range starting from the random seed.
-- Returns the range and the next random seed.
rndRange : Float -> Float -> Float -> (Float, Float)
rndRange from to seed = 
  let rnd = lcg seed
  in ((from + (rnd * (to - from))), rnd)


-------------------------------------------------
-- Update of state
-------------------------------------------------

-- How many pixels per second an object moves if its speed v=1.
pixelsPerSecond : Float
pixelsPerSecond = 7


-- Update the current state of the game.
-- When Won or Lost, wait one second to go to the next state so that accidental space 
--  presses won't immidiately cause the player to go to the next state.
updateGame : Input -> GameState -> GameState
updateGame inp state = case state of
  -- The game is lost, Start again when the player presses play and the time passed is 
  -- more than 1 second.
  Lost time level -> if | inp.space && time > 1 -> (rndStartState inp.rnd 1)
                        | otherwise -> Lost (time + inp.delta) level
  -- The game is Won, continue with the nex level when the player presses play and 
  -- the time passed is more than 1 second.
  Won time level ->  if | inp.space && time > 1 -> (rndStartState inp.rnd (level + 1))
                        | otherwise -> Won (time + inp.delta) level
  -- Begin state of the game
  Begin ->           if | inp.space -> (rndStartState inp.rnd 1)
                        | otherwise -> Begin
  -- The game is currently in progress
  Game fieldstate level -> 
    let newState = updateObjects inp fieldstate
    in if | (shipCollision newState) -> Lost 0 level  -- Lost the game
          | (isNoAstroidLeft newState) -> Won 0 level -- Won the current level
          | otherwise -> Game newState level

-- Check if there are no asteroids left.
-- This is used to check if the current level is won.
isNoAstroidLeft : FieldState -> Bool
isNoAstroidLeft s = (isEmpty s.asteroids)

-- Update all the object of the fieldstate.
updateObjects : Input -> FieldState -> FieldState
updateObjects inp state = 
  let (newShip, bullet) = shootBullet inp state.ship
  in bulletCollisions { 
    state | ship <- (shipUpdate inp newShip),
            asteroids <- (updateAsteroids inp state.asteroids),
            bullets <- cons bullet (updateBullets inp state.bullets) }


-------------------------------------------------
-- Update of ObjState
-------------------------------------------------

-- Update the position of the object according to its velocity and the input.
updatePos : Input -> ObjState a -> ObjState a
updatePos inp state = 
    { state | x <- state.x + cos(state.dir) * state.v * inp.delta * pixelsPerSecond , 
              y <- state.y + sin(state.dir) * state.v * inp.delta * pixelsPerSecond }

-- Update the velocity of the object according to the input.
updateVelocity : Input -> ObjState a -> ObjState a
updateVelocity inp state = { state | v <- state.v + inp.y * inp.delta * 7 }

-- Update the direction of the object according to the input.
updateDir : Input -> ObjState a -> ObjState a
updateDir inp state =
    { state | dir <- state.dir - inp.x * inp.delta * 3}

-- Bounce the object against the sides of the playing field.
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


-------------------------------------------------
-- Update of the ship
-------------------------------------------------

-- Update the state of the ship according to the input.
shipUpdate : Input -> (ShipState -> ShipState)
shipUpdate inp = 
  bounce . 
    (updatePos inp) . 
      (updateVelocity inp) . 
        (updateDir inp)

-- Shoot a bullet from the nose of the ship.
-- Only 1 bullet per second can be fired.
shootBullet : Input -> ShipState -> (ShipState, Maybe BulletState)
shootBullet inp s =
  -- Shoot the bullet if space is pressed and no other bullet is fired in the last second
  if | inp.space && (s.timeLastBullet > 1) -> 
    let (noseX, noseY) = getNoseOfShip s 
    in ({s | timeLastBullet <- 0}, Just { x=noseX, y=noseY, v=s.v+4, dir=s.dir, diam=1 })
     -- don't shoot a bullet, update the last time a bullet was fired
     | otherwise -> ({s | timeLastBullet <- s.timeLastBullet + inp.delta}, Nothing)


-- Get the nose of the ship.
-- Helper function to fire a bullet.
getNoseOfShip : ShipState -> (Float, Float)
getNoseOfShip s = (s.x + cos(s.dir) * 10,
                   s.y + sin(s.dir) * 10)


-------------------------------------------------
-- Update of the bullets
-------------------------------------------------

-- Update the bullet according to the input.
bulletUpdate : Input -> BulletState -> BulletState
bulletUpdate inp state = updatePos inp state

-- Update all the bullets according to the input.
updateBullets : Input -> [BulletState] -> [BulletState]
updateBullets inp blist = 
  filter isOutOfField  -- filter out bullets that are outside of the playing field
         (map (\x -> (bulletUpdate inp x)) blist) -- update each bullet

-- Check if a bullet is out of the playing field.
-- Helper function to update the list of all the bullets.
isOutOfField : BulletState -> Bool
isOutOfField state = 
  if | state.x > 250 || state.x < -250 || state.y > 250 || state.y < -250 -> False
     | otherwise -> True


-------------------------------------------------
-- Update of the asteroids
-------------------------------------------------

-- Update the asteroid according to the input.
asteroidUpdate : Input -> AsteroidState -> AsteroidState
asteroidUpdate inp state = bounce (updatePos inp state)

-- Update all asteroids according to the given input.
-- First let the asteroids bounce of each other and then update their position.
updateAsteroids : Input -> [AsteroidState] -> [AsteroidState]
updateAsteroids inp asteroids = 
  (map (\x -> (asteroidUpdate inp x)) (bounceAllAsteroids asteroids))

-- Bouncing of asteroids
-------------------------------------------------

-- Bounce all asteroids of each other if needed.
bounceAllAsteroids : [AsteroidState] -> [AsteroidState]
bounceAllAsteroids a0s = fst (bounceAllAsteroidsWithAllAsteroids [] a0s)

-- for each pair of asteroids, check if they collide and let them bounce if they do.
-- No pair is checked twice.
-- bounceAllAsteroidsWithAllAsteroids a0s a1s
--    a0s are the asteroids that are checked.
--    a1s are the asteroids that still need to be checked.
-- Return the updated states in a0s.
bounceAllAsteroidsWithAllAsteroids : [AsteroidState] -> [AsteroidState] -> ([AsteroidState], [AsteroidState])
bounceAllAsteroidsWithAllAsteroids a0s a1s = 
  case a1s of
    -- There are remaining asteroids to check.
    (a1::a1stail) -> let (a1tailnew, a1n, _) =  bounceAsteroidWithAsteroids [] a1 a1stail
                     in bounceAllAsteroidsWithAllAsteroids (a0s++[a1n]) a1tailnew
    -- No more asteroids to check.
    otherwise -> (a0s, [])

-- Check if once astroid collides with an other asteroid and let them bounce if they do.
-- bounceAsteroidWithAsteroids a0s a1 a2s 
--    a0s: asteroids that are checked allready.
--    a1: asteroid to check with all asteroids in a2s.
--    a2s: asteroids that still have to be checked.
-- Return the updated states in a0s.
bounceAsteroidWithAsteroids : [AsteroidState] -> AsteroidState -> [AsteroidState] -> ([AsteroidState], AsteroidState, [AsteroidState])
bounceAsteroidWithAsteroids a0s a1 a2s = 
  case a2s of
    -- There are remaining asteroids to check.
    (a2::a2stail) -> let (a1n, a2n) = (checkAsteroidBounce a1 a2)
                     in  bounceAsteroidWithAsteroids (a0s++[a2n]) a1n a2stail
    -- No more asteroids to check.
    otherwise -> (a0s, a1, [])

-- Check between 2 astroids if they collide, and let them bounce if they do,
-- and return their update state.
checkAsteroidBounce : AsteroidState -> AsteroidState -> (AsteroidState , AsteroidState)
checkAsteroidBounce a1 a2 = 
  if | (objCollision a1 a2) -> (asteroidBounce a1 a2)
     | otherwise -> (a1, a2)

-- Let the astroids bounce against each other.
-- Return their updated state.
asteroidBounce : AsteroidState -> AsteroidState -> (AsteroidState , AsteroidState)
asteroidBounce a1 a2 = 
  let colA = atan2 (a1.y - a2.y) (a1.x - a2.x) -- collision angle
      iv1x = a1.v * (cos (a1.dir - colA)) -- initial velocity a1 x
      iv1y = a1.v * (sin (a1.dir - colA)) -- initial velocity a1 y
      iv2x = a2.v * (cos (a2.dir - colA)) -- initial velocity a2 x
      iv2y = a2.v * (sin (a2.dir - colA)) -- initial velocity a2 y
      v1 = (sqrt (iv2x^2 + iv1y^2)) -- new velocity for asteroid 1
      v2 = (sqrt (iv1x^2 + iv2y^2)) -- new velocity for asteroid 2
      dir1 = (atan2 iv1y iv2x) + colA -- new direction for asteroid 1
      dir2 = (atan2 iv2y iv1x) + colA -- new direction for asteroid 2
  in moveAstroidsApart {a1 | v <- v1, dir <- dir1} -- update state a1
                       {a2 | v <- v2, dir <- dir2} -- update state a2

-- Move overlapping asteroids appart.
-- This is used to pull asteroids that have bounced but still overlap apart.
moveAstroidsApart : AsteroidState -> AsteroidState -> (AsteroidState , AsteroidState)
moveAstroidsApart a1 a2 = 
  if | (objCollision a1 a2) -> moveAstroidsApart { a1 | x <- a1.x + cos(a1.dir) * a1.v * 0.5, y <- a1.y + sin(a1.dir) * a1.v * 0.5} { a2 | x <- a2.x + cos(a2.dir) * a2.v * 0.5, y <- a2.y + sin(a2.dir) * a2.v * 0.5}
     | otherwise -> (a1, a2)

-- Collisions with bullets or ship
-------------------------------------------------

-- Let the astroids collide with the bullets and return the update field state.
bulletCollisions : FieldState -> FieldState
bulletCollisions state = 
  -- filter: Filter out elements which do not satisfy the predicate
    {state | asteroids <- filter (\x -> not (objectHit x state.bullets)) state.asteroids,
             bullets <- filter (\x -> not (objectHit x state.asteroids)) state.bullets}

-- Return true if the ship collides with an asteroid.
shipCollision : FieldState -> Bool
shipCollision state = objectHit state.ship state.asteroids

-- Return true if the given object collides with an object in the given list.
objectHit : ObjState a -> [ObjState b] -> Bool
objectHit x ys = any (\z -> objCollision x z) ys

-- Return true if the given objects collide with each other.
objCollision : ObjState a -> ObjState b -> Bool
objCollision s1 s2 = if | (dist s1 s2) < s1.diam + s2.diam -> True
                        | otherwise -> False

-- Return the distance between the 2 objects.
dist : ObjState a -> ObjState b -> Float
dist s1 s2 = sqrt( (s1.x-s2.x)^2 + (s1.y-s2.y)^2 )


-------------------------------------------------
-- Helper functions
-------------------------------------------------

-- Return true if the given list is empty.
isEmpty : [a] -> Bool
isEmpty l = (length l) == 0


-------------------------------------------------
-- DEBUG
-------------------------------------------------

-- The debugging signal to show.
debugInfo : Signal Input
debugInfo = input -- Whatever info you want to show for debugging.

-- The debuggin signal as an element to show.
debugEl : Signal Element
debugEl = lift asText debugInfo


-------------------------------------------------
-- MAIN
-------------------------------------------------
main : Signal Element
main = lift2 above game debugEl
