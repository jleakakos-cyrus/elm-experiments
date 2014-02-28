import Keyboard
import Window

delta                        = lift inSeconds (fps 50)
(gameWidth, halfGameWidth)   = (600, 300)
(gameHeight, halfGameHeight) = (400, 200)
ballSize                     = 15
boundrySize                  = 2 * ballSize

-- Model
type Input = { dt: Time, spacebar: Bool, lr: Int, ud: Int }

type Ball = { x: Float, y: Float, dx: Int, dy: Int }
type Paddle = { x: Float, y: Float, dy: Int }

type Game = { bs: [Ball], p: Paddle }

defaultInput : Input
defaultInput = { dt = 0, spacebar = False, lr = 0, ud = 0 }

defaultPaddle : Paddle
defaultPaddle = { x = 50, y = 0, dy = 0 }

defaultBall : Ball
defaultBall = { x = 0, y = 0, dx = 0, dy = 1 }

secondBall : Ball
secondBall = { x = 0, y = 0, dx = 1, dy = 0 }

thirdBall : Ball
thirdBall = { x = 0, y = 0, dx = -1, dy = 1 }

defaultGame : Game
defaultGame = { bs = [defaultBall, secondBall, thirdBall],
                p = defaultPaddle }

input = sampleOn delta (lift4 Input delta
                              Keyboard.space 
                              (lift .x Keyboard.arrows)
                              (lift .y Keyboard.arrows))
                              

-- Update
stepBall : Time -> Bool -> Int -> Int -> Ball -> Ball
stepBall dt spacebar lr ud ({x, y, dx, dy} as b) =
  let dx' = if lr /= 0 then lr else dx
      dy' = if ud /= 0 then ud else dy
      x' = x + (toFloat dx' * 500 * dt)
      y' = y + (toFloat dy' * 500 * dt)
      -- c' = if spacebar then red else blue
  in if spacebar
     then {b | dx <- 0,
               dy <- 0}
     else { x = if (x' <= (-halfGameWidth + boundrySize)) ||
                   (x' >= (halfGameWidth - boundrySize))
                then x
                else x',
            y = if (y' <= (-halfGameHeight + boundrySize)) ||
                   (y' >= (halfGameHeight - boundrySize))
                then y
                else y',
            dx = if (x' <= (-halfGameWidth + boundrySize)) ||
                   (x' >= (halfGameWidth - boundrySize))
                 then -dx
                 else dx',
            dy = if (y' <= (-halfGameHeight + boundrySize)) ||
                   (y' >= (halfGameHeight - boundrySize))
                 then -dy
                 else dy'}

stepPaddle : Time -> Bool -> Int -> Paddle -> Paddle
stepPaddle dt spacebar ud ({x, y, dy} as p) =
  let dy' = if ud /= 0 then ud else dy
      y' = y + (toFloat dy' * 500 * dt)
  in if spacebar
    then { p | dy <- 0 }
    else { p | y <- if (y' <= (-halfGameHeight + boundrySize)) ||
                      (y' >= (halfGameHeight - boundrySize))
                   then y
                   else y',
               dy <- if (y' <= (-halfGameHeight + boundrySize)) ||
                       (y' >= (halfGameHeight - boundrySize))
                    then -dy
                    else dy'}

stepGame : Input -> Game -> Game
stepGame {dt, spacebar, lr, ud} ({bs, p} as game) =
  {game | bs <- map (stepBall dt spacebar lr ud) bs,
          p <- stepPaddle dt spacebar ud p}

gameState = foldp stepGame defaultGame input


-- Display
pongGreen = rgb 60 100 60
textGreen = rgb 160 200 160
txt f = text . f . monospace . Text.color textGreen . toText

display : (Int, Int) -> Input -> Game -> Element
display (w, h) input ({bs, p} as game) = 
  flow down 
  [
    container w (h-100) middle <|
      collage gameWidth gameHeight <|
        [ rect gameWidth gameHeight |> filled pongGreen,
          move (.x p, .y p) <| filled grey <| rect 20 100] ++ 
        map (\b -> move (.x b, .y b) <| filled grey <| circle ballSize) bs
        
  ]


main = lift3 display Window.dimensions input gameState

main2 = lift asText <| lift4 Input delta
                                   Keyboard.space 
                                   (lift .x Keyboard.arrows)
                                   (lift .y Keyboard.arrows)
                                     
