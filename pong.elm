import Keyboard
import Window

delta                        = lift inSeconds (fps 50)
(gameWidth, halfGameWidth)   = (600, 300)
(gameHeight, halfGameHeight) = (400, 200)
ballSize                     = 15
boundrySize                  = 2 * ballSize

-- Model
type Input = { dt: Time, spacebar: bool, lr: Int, ud: Int }

type Ball = { x: Float, y: Float, dx: Int, dy: Int }

type Game = { b: Ball }

defaultInput : Input
defaultInput = { dt = 0, spacebar = False, lr = 0, ud = 0 }

defaultBall : Ball
defaultBall = { x = 0, y = 0, dx = 0, dy = 0 }

defaultGame : Game
defaultGame = { b = defaultBall }

input = sampleOn delta (lift4 Input delta
                              Keyboard.space 
                              (lift .x Keyboard.arrows)
                              (lift .y Keyboard.arrows))
                              

-- Update
stepBall : Time -> Bool -> Int -> Int -> Ball -> Ball
stepBall dt spacebar lr ud ({x, y, dx, dy} as b) =
  let dx' = if lr /= 0 then lr else dx
      dy' = if ud /= 0 then ud else dy
      x' = x + (toFloat dx' * 300 * dt)
      y' = y + (toFloat dy' * 300 * dt)
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
            dx = dx',
            dy = dy' }

stepGame : Input -> Game -> Game
stepGame {dt, spacebar, lr, ud} ({b} as game) =
  {game | b <- stepBall dt spacebar lr ud b}

gameState = foldp stepGame defaultGame input


-- Display
pongGreen = rgb 60 100 60
textGreen = rgb 160 200 160
txt f = text . f . monospace . Text.color textGreen . toText

display : (Int, Int) -> Input -> Game -> Element
display (w, h) input ({b} as game) = 
  flow down 
  [
    asText <| "x: " ++ show (.x b) ++
              ", y: " ++ show (.y b) ++
              ", dx: " ++ show (.dx b) ++
              ", dy: " ++ show (.dy b),
    container w (h-100) middle <|
      collage gameWidth gameHeight <|
        [ filled pongGreen <| rect gameWidth gameHeight,
          move (.x b, .y b) <| filled grey <| circle ballSize
        ]
  ]


main = lift3 display Window.dimensions input gameState

main2 = lift asText <| lift4 Input delta
                                   Keyboard.space 
                                   (lift .x Keyboard.arrows)
                                   (lift .y Keyboard.arrows)
                                     
