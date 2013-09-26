import Keyboard
import Window

delta                        = lift inSeconds (fps 50)
(gameWidth, halfGameWidth)   = (600, 300)
(gameHeight, halfGameHeight) = (400, 200)
ballSize                     = 15

-- Model
type Input = { dt: Time, spacebar: bool, lr: Int, ud: Int }

type Ball = { x: Float, y: Float, c: Color }

type Game = { b: Ball }

defaultInput : Input
defaultInput = { dt = 0, spacebar = False, lr = 0, ud = 0 }

defaultBall : Ball
defaultBall = { x = 0, y = 0, c = blue}

defaultGame : Game
defaultGame = { b = defaultBall }

input = sampleOn delta (lift4 Input delta
                              Keyboard.space 
                              (lift .x Keyboard.arrows)
                              (lift .y Keyboard.arrows))
                              

-- Update
stepBall : Time -> Bool -> Int -> Int -> Ball -> Ball
stepBall dt spacebar lr ud {x, y, c} =
  let x' = x + (toFloat lr * 1000 * dt)
      y' = y + (toFloat ud * 1000 * dt)
      c' = if spacebar then red else blue
  in { x = x', y = y', c = c' }

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
    asText input,
    container w (h-100) middle <|
      collage gameWidth gameHeight <|
        [ filled pongGreen <| rect gameWidth gameHeight,
          move (.x b, .y b) <| filled (.c b) <| circle ballSize
        ]
  ]


main = lift3 display Window.dimensions input gameState

main2 = lift asText <| lift4 Input delta
                                   Keyboard.space 
                                   (lift .x Keyboard.arrows)
                                   (lift .y Keyboard.arrows)
                                     
