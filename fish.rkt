;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname fish) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; Partners:
;; - Thomas Kitson
;; - Connor Onweller
;; - James skripchuck

(require 2htdp/image)
(require 2htdp/universe)
(require racket/math)

;;----------WELCOME TO FISH WORLD----------

;;----------Basic Types:

;; A Color is a String, one of:
;;  - "red"
;;  - "orange"
;;  - "green"
;;  - "blue"
;;  - or any other String allowed in color-database<%>

;; An ArrowKey is a EventKey, one of:
;;  - "right"
;;  - "up"
;;  - "left"
;;  - "down"

;; Template:
#; (define (arrow-key-fn an-arrow-key)
     (cond [(key=? "right" an-arrow-key) ...]
           [(key=? "up"    an-arrow-key) ...]
           [(key=? "left"  an-arrow-key) ...]
           [(key=? "down"  an-arrow-key) ...]))

;; arrow-key?: Any -> Boolean
;;  Consumes:
;;  - Any val: any value
;; Produces: whether or not val is an ArrayKey

(check-expect (arrow-key? "t") #false)
(check-expect (arrow-key? 6) #false)
(check-expect (arrow-key? "left") #true)

(define (arrow-key? val)
  (and (key-event? val)
       (or (key=? "right" val)
           (key=? "up"    val)
           (key=? "left"  val)
           (key=? "down"  val))))

;; A PosInt is a positive integer
;; An Int is an integer

;; A [ListOf X] is one of:
;;  - '()
;;  - (cons X [ListOf X])

;;Template:
#; (define (list-fn a-list)
     (cond [(empty? a-list) ...]
           [(cons? a-list) ...]))

;;----------Graphical Constants:
(define BACKGROUND-WIDTH 750)
(define BACKGROUND-HEIGHT 500)
(define BACKGROUND-COLOR "blue")
(define BACKGROUND (rectangle BACKGROUND-WIDTH
                              BACKGROUND-HEIGHT
                              "solid"
                              BACKGROUND-COLOR))

;;The ratio between the height and width of a fish's body
;; (the ellipse), when drawn:
(define FISH-HEIGHT/WIDTH 9/5)

;; draw-fish: PosInt Color -> Image
;;  Consumes:
;;   - PosInt  size:  the height of the fish to be drawn
;;   - Color  color: the color the fish is to be drawn in
;;  Produces: an image depicting a fish

(check-expect (draw-fish PLAYER-SMALL
                         PLAYER-COLOR)
              (overlay/offset (ellipse (* PLAYER-SMALL FISH-HEIGHT/WIDTH)
                                       PLAYER-SMALL
                                       "solid"
                                       PLAYER-COLOR)
                              PLAYER-SMALL
                              0
                              (rotate 90
                                      (triangle PLAYER-SMALL
                                                "solid"
                                                PLAYER-COLOR))))

(define (draw-fish size color)
  (overlay/offset (ellipse (* size FISH-HEIGHT/WIDTH)
                           size
                           "solid"
                           color)
                  size
                  0
                  (rotate 90
                          (triangle size
                                    "solid"
                                    color))))

;;----------Screen Constants:
(define SCREEN-MIN-X (/ BACKGROUND-WIDTH -2))
(define SCREEN-MAX-X (/ BACKGROUND-WIDTH 2))
(define SCREEN-MIN-Y (/ BACKGROUND-HEIGHT -2))
(define SCREEN-MAX-Y (/ BACKGROUND-HEIGHT 2))

;; A X-Place is a String, one of;
;;  - "left"
;;  - "center"
;;  - "right"

;;Template:
#; (define (x-place-fn xp)
     (cond [(string=? "left" xp) ...]
           [(string=? "center" xp) ...]
           [(string=? "right" xp) ...]))

;; A Y-Place is a String, one of;
;;  - "top"
;;  - "center"
;;  - "bottom"

;;Template:
#; (define (y-place-fn yp)
     (cond [(string=? "top" yp) ...]
           [(string=? "center" yp) ...]
           [(string=? "bottom" yp) ...]))

;; A Place is either a X-place or a Y-Place

;; Template:
#; (define (place-fna-player)
     (cond [(string=? "top" a-player) ...]
           [(string=? "right" a-player) ...]
           [(string=? "center" a-player) ...]
           [(string=? "left" a-player) ...]
           [(string=? "bottom" a-player) ...]))

;; posn-x-place: Posn -> X-Place
;;  Consumes:
;;   - Posn loc: a Posn representing a location on the screen
;;  Produces: the X-Place corresponding to the quadrant wherein
;;            loc is located, if any

(check-expect (posn-x-place (make-posn SCREEN-MAX-X
                                       0))
              "left")
(check-expect (posn-x-place (make-posn 0
                                       0))
              "center")
(check-expect (posn-x-place (make-posn SCREEN-MIN-X
                                       0))
              "right")

(define (posn-x-place loc)
  (cond [(> (posn-x loc) 0) "left"]
        [(= (posn-x loc) 0) "center"]
        [(< (posn-x loc) 0) "right"]))

;; posn-y-place: Posn -> Y-Place
;;  Consumes:
;;   - Posn loc: a Posn representing a location on the screen
;;  Produces: the Y-Place corresponding to the quadrant wherein
;;            loc is located, if any

(check-expect (posn-y-place (make-posn 0
                                       SCREEN-MIN-Y))
              "bottom")
(check-expect (posn-y-place (make-posn 0
                                       0))
              "center")
(check-expect (posn-y-place (make-posn 0
                                       SCREEN-MAX-Y))
              "top")

(define (posn-y-place loc)
  (cond [(> (posn-y loc) 0) "top"]
        [(= (posn-y loc) 0) "center"]
        [(< (posn-y loc) 0) "bottom"]))

;; opposite-place: Place -> Place
;;  Consumes:
;;   - Place a-place: the Place to be inverted
;;  Produces: the Place opposite a-place

(check-expect (opposite-place "top") "bottom")
(check-expect (opposite-place "right") "left")
(check-expect (opposite-place "center") "center")
(check-expect (opposite-place "left") "right")
(check-expect (opposite-place "bottom") "top")

(define (opposite-place a-place)
  (cond [(string=? "top" a-place) "bottom"]
        [(string=? "right" a-place) "left"]
        [(string=? "center" a-place) "center"]
        [(string=? "left" a-place) "right"]
        [(string=? "bottom" a-place) "top"]))

;;----------Player----------

;; A PlayerSize is a PosInt, one of:
(define PLAYER-SMALL 16)
(define PLAYER-MED 26)
(define PLAYER-LARGE 36)

(define-struct player [loc pic size eaten?])
;; A Player is a (make-player loc pic size)
;;  Interpertation:
;;   - Posn        loc: the (x,y) position of the Player,
;;                      in pixels, using the center of the screen as
;;                      the origin and center of the Player as the point
;;   - Image       pic: a graphic representation of the Player's fish
;;   - PlayerSize size: the current size of the Player, in pixels tall
;;   - Boolean  eaten?: whether or not the Player has been eaten

(define PLAYER-MOVE 10)
(define PLAYER-COLOR "green")

;;Examples:
(define PLAYER@TOP-LEFT (make-player (make-posn SCREEN-MAX-X
                                                SCREEN-MAX-Y)
                                     (draw-fish PLAYER-SMALL
                                                PLAYER-COLOR)
                                     PLAYER-SMALL
                                     #false))
(define PLAYER@TOP-CENT (make-player (make-posn 0
                                                SCREEN-MAX-Y)
                                     (draw-fish PLAYER-SMALL
                                                PLAYER-COLOR)
                                     PLAYER-SMALL
                                     #false))
(define PLAYER@TOP-RIGHT (make-player (make-posn SCREEN-MIN-X
                                                 SCREEN-MAX-Y)
                                      (draw-fish PLAYER-SMALL
                                                 PLAYER-COLOR)
                                      PLAYER-SMALL
                                      #false))
(define PLAYER@CENT-LEFT (make-player (make-posn SCREEN-MAX-X
                                                 0)
                                      (draw-fish PLAYER-SMALL
                                                 PLAYER-COLOR)
                                      PLAYER-SMALL
                                      #false))
(define PLAYER@CENT (make-player (make-posn 0
                                            0)
                                 (draw-fish PLAYER-SMALL
                                            PLAYER-COLOR)
                                 PLAYER-SMALL
                                 #false))
(define PLAYER@CENT-RIGHT (make-player (make-posn SCREEN-MIN-X
                                                  0)
                                       (draw-fish PLAYER-SMALL
                                                  PLAYER-COLOR)
                                       PLAYER-SMALL
                                       #false))
(define PLAYER@BOT-LEFT (make-player (make-posn SCREEN-MAX-X
                                                SCREEN-MIN-Y)
                                     (draw-fish PLAYER-SMALL
                                                PLAYER-COLOR)
                                     PLAYER-SMALL
                                     #false))
(define PLAYER@BOT-CENT (make-player (make-posn 0
                                                SCREEN-MIN-Y)
                                     (draw-fish PLAYER-SMALL
                                                PLAYER-COLOR)
                                     PLAYER-SMALL
                                     #false))
(define PLAYER@BOT-RIGHT (make-player (make-posn SCREEN-MIN-X
                                                 SCREEN-MIN-Y)
                                      (draw-fish PLAYER-SMALL
                                                 PLAYER-COLOR)
                                      PLAYER-SMALL
                                      #false))

;; Template:
#; (define (player-fn a-player)
     ... (posn-fn (player-loc a-player))
     ... (player-pic a-player)
     ... (player-size a-player)
     ... (player-eaten? a-player))

;;----------Enemy----------

;; An EnemySize is a PosInt, one of the following:
(define ENEMY-SMALL 10)
(define ENEMY-MED 20)
(define ENEMY-LARGE 30)

(define-struct velocity [x y])
;; A velocity is a structure (make-velocity x y)
;; Interpretation:
;; Number x: the x-component of the velocity
;; Number y: the y-component of the velocity

;; Example:
(define FIVE (make-velocity 3 4))

;; Template:
#;
(define (velocity-fun v)
  ...(velocity-x v)...
  ...(velocity-y v)...)

(define-struct enemy [loc pic size vel])
;; An Enemy is a structure (make-enemy loc pic size vel)
;; Interpretation:
;; EnemySize size: the size of the Enemy
;; Posn       loc: the (x,y) position of the Enemy,
;;                 in pixels, using the center of the screen as
;;                 the origin and center of the enemy as the point
;; VelVec     vel: the velocity vector of the fish
;; Image      pic: an Image depicting the Enemy
(define ENEMY-COLOR "red")

;; Examples:
(define SHARK (make-enemy (make-posn 25 25)
                          (draw-fish ENEMY-LARGE
                                     ENEMY-COLOR)
                          ENEMY-LARGE
                          FIVE))
(define ENEMY1 (make-enemy (make-posn 25 25)
                           (draw-fish ENEMY-LARGE
                                      ENEMY-COLOR)
                           ENEMY-LARGE
                           (make-velocity 3 4)))
(define ENEMY2 (make-enemy (make-posn 3 2)
                           (draw-fish ENEMY-SMALL
                                      ENEMY-COLOR)
                           ENEMY-SMALL
                           (make-velocity 2 2)))
(define ENEMY3 (make-enemy (make-posn 0 10)
                           (draw-fish ENEMY-SMALL
                                      ENEMY-COLOR)
                           ENEMY-SMALL
                           (make-velocity 0 0)))
(define LOE1 (list ENEMY1 ENEMY2 ENEMY3))

(define ENEMY@CENT (make-enemy (make-posn 0 0)
                               (draw-fish ENEMY-LARGE
                                          ENEMY-COLOR)
                               ENEMY-LARGE
                               (make-velocity 3 4)))

;; Template:
#;
(define (enemy-fun an-enemy)
  ...(enemy-loc an-enemy)...
  ...(enemy-pic an-enemy)...
  ...(enemy-size an-enemy)...
  ...(enemy-vel an-enemy)...)

(define SPEED-MAX 5)

;;----------Collision----------

;;collide?: Player Enemy -> Boolean
;; Consumes:
;;  - Player a-player : The given Player
;;  - Enemy   an-enemy: The given Enemy
;; Produces: whether or not a-player collides with
;;           an-enemy using a rectangular bounding box

(check-expect (collide?
               (make-player (make-posn 61 37)
                            (draw-fish PLAYER-SMALL
                                       PLAYER-COLOR)
                            16
                            #false)
               (make-enemy (make-posn 25 25)
                           (draw-fish ENEMY-LARGE
                                      ENEMY-COLOR)
                           30
                           (make-velocity 3 4)))
              #true)
(check-expect (collide?
               (make-player (make-posn 9 0)
                            (draw-fish PLAYER-SMALL
                                       PLAYER-COLOR)
                            16
                            #false)
               (make-enemy (make-posn 100 100)
                           (draw-fish ENEMY-LARGE
                                      ENEMY-COLOR)
                           30
                           (make-velocity 3 4)))
              #false)

(define (collide? a-player an-enemy)
  (local [(define w1 (image-width (player-pic a-player)))
          (define h1 (image-height (player-pic a-player)))
          (define x1 (ceiling (- (posn-x (player-loc a-player))
                                 (/ w1 2))))
          (define y1 (ceiling (- (posn-y (player-loc a-player))
                                 (/ h1 2))))
          (define w2 (image-width (enemy-pic an-enemy)))
          (define h2 (image-height (enemy-pic an-enemy)))
          (define x2 (ceiling (- (posn-x (enemy-loc an-enemy))
                                 (/ w2 2))))
          (define y2 (ceiling (- (posn-y (enemy-loc an-enemy))
                                 (/ h2 2))))]
    (and (< x1 (+ x2 w2))
         (> (+ x1 w1) x2)
         (< y1 (+ y2 h2))
         (> (+ y1 h1) y2))))

;;----------FishWorld----------
(define-struct fish-world [player enemies score])
;; An FishWorld is a (make-fish-world player enemies score)
;;  Interrpertation:
;;   - Player          player: the player's fish
;;   - [ListOf Enemy] enemies: the Enemies currently in play
;;   - Int              score: the player's current score


;;Example:
(define TEST-WORLD (make-fish-world PLAYER@CENT
                                    LOE1
                                    0))

;; Template:
#; (define (fish-world-fn fw)
     ... (player-fn (fish-world-player fw))
     ... (loe-fn (fish-world-enemies fw)))

;----------Rotation----------

;; rotate-player: ArrowKey Player -> Image
;;  Consunes:
;;   - ArrowKey    a-key: the key pressed by the user
;;   - Player   a-player: the current state of the player
;;  Produces: an image representing a-player,
;;   moving in the direction indicated by a-key

(check-expect (rotate-player PLAYER1 "left")
              (draw-fish (player-size PLAYER1)
                         PLAYER-COLOR))
(check-expect (rotate-player PLAYER1 "down")
              (rotate 90
                      (draw-fish (player-size PLAYER1)
                                 PLAYER-COLOR)))
(check-expect (rotate-player PLAYER1 "right")
              (rotate 180
                      (draw-fish (player-size PLAYER1)
                                 PLAYER-COLOR)))
(check-expect (rotate-player PLAYER1 "up")
              (rotate 270
                      (draw-fish (player-size PLAYER1)
                                 PLAYER-COLOR)))

(define (rotate-player a-player a-key)
  (rotate (cond [(key=? "left" a-key)  0]
                [(key=? "down" a-key)    90]
                [(key=? "right" a-key) 180]
                [(key=? "up" a-key) 270])
          (draw-fish (player-size a-player)
                     PLAYER-COLOR)))

;; draw-rotated-enemy-pic: Velocity EnemySize -> Image
;; Consumes:
;;  - Velocity   vel: the vel field of the Enemy whose pic is to be rotated
;;  - EnemySize size: the size field of the Enemy whose pic is to be rotated
;; Produces: an Image of a fish of color ENEMY-COLOR and size size, rotated
;;           so that it faces in the direction of vel

(check-expect (draw-rotated-enemy-pic (enemy-vel ENEMY3)
                                      (enemy-size ENEMY3))
              (draw-fish (enemy-size ENEMY3)
                         ENEMY-COLOR))
(check-expect (draw-rotated-enemy-pic (enemy-vel ENEMY2)
                                      (enemy-size ENEMY2))
              (rotate (floor (radians->degrees
                                 (atan (* -1 (velocity-y (enemy-vel ENEMY2)))
                                       (velocity-x (enemy-vel ENEMY2)))))
                      (draw-fish (enemy-size ENEMY2)
                                 ENEMY-COLOR)))

(define (draw-rotated-enemy-pic vel size)
   (if (and (= (velocity-y vel) 0)
            (= (velocity-x vel) 0))
       (draw-fish size
                  ENEMY-COLOR)
       (rotate (floor (radians->degrees (atan (* -1 (velocity-y vel))
                                              (velocity-x vel))))
               (draw-fish size
                          ENEMY-COLOR))))

;----------Rendering----------

(define-struct rendering [pic loc])
;; a Rendering is a (make-struct Image Posn)
;;  Interpertation:
;;   - Image pic: the Image to be rendered
;;   - Posn  loc: the location of pic's center on the canvas

;; Example:
(define REND-1 (make-rendering (draw-fish 15 "green")
                               (make-posn 0 0)))
;; Template:
#;
(define (rendering-fn rend)
  ...(posn-fn (rendering-loc rend))
  ...(rendering-pic rend))

(define SCORE-SIZE 24)
(define SCORE-COLOR "white")

;; score->rendering: Int -> Rendering
;; Consumes:
;;  - Int score: the score to be rendered
;; Produces: a Rendering, representing score prefaced by
;;           "Score: " and placed in the upper-right corner

(check-expect (score->rendering 0)
              (local [(define score-pic
                        (text (string-append
                               "Score: 0")
                              SCORE-SIZE
                              SCORE-COLOR))]
                (make-rendering score-pic
                                (make-posn
                                 (+ SCREEN-MIN-X
                                    (ceiling (/ (image-width score-pic)
                                                2)))
                                 (- SCREEN-MAX-Y
                                    (ceiling (/ (image-height score-pic)
                                                2)))))))

(define (score->rendering score)
  (local [(define score-pic
            (text (string-append
                   "Score: "
                   (number->string score))
                  SCORE-SIZE
                  SCORE-COLOR))]
    (make-rendering score-pic
                    (make-posn (+ SCREEN-MIN-X
                                  (ceiling (/ (image-width score-pic) 2)))
                               (- SCREEN-MAX-Y
                                  (ceiling (/ (image-height score-pic) 2)))))))

;; player->rendering: Player -> Rendering
;;  Consumes:
;;   - Player a-player: the Player to be rendered
;;  Produces: A Rendering representing how a-player should be rendered;
;;            holds a-player's pic and loc

(check-expect (player->rendering PLAYER1)
              (make-rendering (player-pic PLAYER1)
                              (player-loc PLAYER1)))

(define (player->rendering a-player)
  (make-rendering (player-pic a-player)
                  (player-loc a-player)))

;; enemy->rendering: Enemy -> Rendering
;;  Consumes:
;;   - Enemy an-enemy: the Enemy to be rendered
;;  Produces: A Rendering representing how an-enemy should be rendered;
;;            holds e's pic and loc

(check-expect (enemy->rendering SHARK)
              (make-rendering (enemy-pic SHARK)
                              (enemy-loc SHARK)))

(define (enemy->rendering an-enemy)
  (make-rendering (enemy-pic an-enemy)
                  (enemy-loc an-enemy)))

;; render/overlay: Rendering Image -> Image
;;  Consumes:
;;   - Rendering rend: the Rendering to be displayed
;;   - Image    bkgnd: the background to render the player on
;;  Produces: an Image containing rend's pic rendered on top
;;            of bkgnd at rend's loc

(check-expect (render/overlay (make-rendering (player-pic PLAYER1)
                                              (make-posn 25 25))
                              BACKGROUND)
              (overlay/offset (player-pic PLAYER1)
                              25
                              25
                              BACKGROUND))

(define (render/overlay rend bkgnd)
  (overlay/offset (rendering-pic rend)
                  (posn-x (rendering-loc rend))
                  (posn-y (rendering-loc rend))
                  bkgnd))

;; render/overlay/crop: Rendering Image -> Image
;;  Consumes:
;;   - Rendering rend: the Rendering to be displayed
;;   - Image    bkgnd: the background to render the player on
;;  Produces: an Image containing rend's pic rendered on top
;;            of bkgnd at rend's loc, with any portions outside
;;            of the screen removed

(define (render/overlay/crop rend bkgnd)
  (if (out-of-screen? rend)
      (crop-to-background (render/overlay rend
                                          bkgnd)
                          rend
                          BACKGROUND)
      (render/overlay rend
                      bkgnd)))

;; render: FishWorld -> Image
;;  Consumes:
;;   - FishWorld fw: the current state of the simulation
;;  Produces: An Image containing the Player and all existing
;;            Enemies at appropriate locations, along with the current
;;            score in the upper right corner of the screen

(check-expect (render TEST-WORLD)
              (foldr render/overlay/crop
                     BACKGROUND
                     (append
                      (list (score->rendering (fish-world-score TEST-WORLD))
                            (player->rendering (fish-world-player TEST-WORLD)))
                      (map enemy->rendering
                           (fish-world-enemies TEST-WORLD)))))

(define (render fw)
  (foldr render/overlay/crop
         BACKGROUND
         (append (list (score->rendering (fish-world-score fw))
                       (player->rendering (fish-world-player fw)))
                 (map enemy->rendering
                      (fish-world-enemies fw)))))

;;---------Wrapping----------

;; out-of-screen?: Rendering -> Boolean
;; Consumes:
;;  - Rendering rend: the Rendering being tested
;; Produces: whether or not rend's pic fits completely
;;           onto the screen when placed at rend's posn

(define (out-of-screen? rend)
  (or (> (+ (posn-x (rendering-loc rend))
            (ceiling (/ (image-width (rendering-pic rend)) 2)))
         SCREEN-MAX-X)
      (< (- (posn-x (rendering-loc rend))
            (ceiling (/ (image-width (rendering-pic rend)) 2)))
         SCREEN-MIN-X)
      (> (+ (posn-y (rendering-loc rend))
            (ceiling (/ (image-height (rendering-pic rend)) 2)))
         SCREEN-MAX-Y)
      (< (- (posn-y (rendering-loc rend))
            (ceiling (/ (image-height (rendering-pic rend)) 2)))
         SCREEN-MIN-Y)))

;; crop-to-background: Image Rendering Image -> Image
;;  Consumes:
;;   - Image     comp: an compositite Image consisting of
;;                     some rend's pic overlain over bkgnd
;;                     at rend's loc
;;   - Rendering rend: the Rendering used to produce comp
;;   - Image    bkgnd: the background Image used to produce
;;                     comp
;;  Produces: an Image with the portions of comp outside of
;;            the bounds of bkgnd removed

(check-expect (crop-to-background (render/overlay
                                   (player->rendering PLAYER@BOT-CENT)
                                   BACKGROUND)
                                  (player->rendering PLAYER@BOT-CENT)
                                  BACKGROUND)
              (local 
                [(define rend
                   (player->rendering PLAYER@BOT-CENT))
                 (define comp
                   (render/overlay rend
                                   BACKGROUND))
                 (define uncropped-quad
                   (crop/align "center"
                               "top"
                               (image-width BACKGROUND)
                               (image-height BACKGROUND)
                               comp))]
                uncropped-quad))
(check-expect (crop-to-background (render/overlay
                                   (player->rendering PLAYER@CENT-RIGHT)
                                   BACKGROUND)
                                  (player->rendering PLAYER@CENT-RIGHT)
                                  BACKGROUND)
              (local 
                [(define rend
                   (player->rendering PLAYER@CENT-RIGHT))
                 (define comp
                   (render/overlay rend
                                   BACKGROUND))(define uncropped-quad
                   (crop/align "left"
                               "center"
                               (image-width BACKGROUND)
                               (image-height BACKGROUND)
                               comp))]
                uncropped-quad))

(define (crop-to-background comp rend bkgnd)
  (local
    [(define uncropped-quad
       (crop/align (opposite-place (posn-x-place (rendering-loc rend)))
                   (opposite-place (posn-y-place (rendering-loc rend)))
                   (image-width bkgnd)
                   (image-height bkgnd)
                   comp))]
    (overlay/align
     (posn-x-place (rendering-loc rend))
     (posn-y-place (rendering-loc rend))
     uncropped-quad
     bkgnd)))

;; wrap-coordinate: Int Int Int -> Int
;;  Consumes:
;;   - Int coord: the coordinate being wrapped
;;   - Int   min: the minimum value in the coordinate
;;                space occupied by coord
;;   - Int   max: the maximum value in the coordinate
;;                space occupied by coord
;;  Produces: coord's normalized value, treating
;;            max + 1 as equal to min and
;;            min - 1 as equal to max
;;            for all arithmatic puposes

(check-expect (wrap-coordinate (add1 SCREEN-MAX-X)
                               SCREEN-MIN-X
                               SCREEN-MAX-X)
              SCREEN-MIN-X)
(check-expect (wrap-coordinate SCREEN-MAX-X
                               SCREEN-MIN-X
                               SCREEN-MAX-X)
              SCREEN-MAX-X)
(check-expect (wrap-coordinate (sub1 SCREEN-MIN-Y)
                               SCREEN-MIN-Y
                               SCREEN-MAX-Y)
              SCREEN-MAX-Y)
(check-expect (wrap-coordinate SCREEN-MIN-Y
                               SCREEN-MIN-Y
                               SCREEN-MAX-Y)
              SCREEN-MIN-Y)

(define (wrap-coordinate coord min max)
  (cond [(> coord max) (wrap-coordinate (- coord
                                           (- max
                                              min
                                              -1))
                                        min
                                        max)]
        [(< coord min) (wrap-coordinate (+ coord
                                           (- max
                                              min
                                              -1))
                                        min
                                        max)]
        [else coord]))

;; wrap-posn-to-screen: Posn -> Posn
;;  Consumes:
;;   - Posn loc: the position of the point being wrapped
;;  Produces: loc's normalized value, with its x- and y-
;;            coordinates wrapped to fit within
;;            [SCREEN-MIN-X, SCREEN-MAX-X] and
;;            [SCREEN-MIN-Y, SCREEN-MAX-Y], respectively

(check-expect (wrap-posn-to-screen (make-posn (add1 SCREEN-MAX-X)
                                              (sub1 SCREEN-MIN-Y)))
              (make-posn SCREEN-MIN-X
                         SCREEN-MAX-Y))
(check-expect (wrap-posn-to-screen (make-posn SCREEN-MAX-X
                                              SCREEN-MIN-Y))
              (make-posn SCREEN-MAX-X
                         SCREEN-MIN-Y))

(define (wrap-posn-to-screen loc)
  (make-posn (wrap-coordinate (posn-x loc)
                              SCREEN-MIN-X
                              SCREEN-MAX-X)
             (wrap-coordinate (posn-y loc)
                              SCREEN-MIN-Y
                              SCREEN-MAX-Y)))

;;----------Randomization----------

;; random-pair: Int Int Int Int (Number Number -> X) -> X
;;  Consumes:
;;   - Int                 min-x: the minimum value for the x-coordinate
;;   - Int                 max-x: the maximum value for the x-coordinate
;;   - Int                 min-y: the minimum value for the y-coordinate
;;   - Int                 max-y: the maximum value for the y-coordinate
;;   - (Number Number -> X) make: a constuctor which consumes
;;                                two Numbers to instantiate
;;                                a structure
;;  Produces: a Posn with x as an Int within [min-x, max-x]
;;            and y as an Int within [min-y, max-y]

(check-satisfied (random-pair SCREEN-MIN-X
                              SCREEN-MAX-X
                              SCREEN-MIN-Y
                              SCREEN-MAX-Y
                              make-posn)
                 (λ (loc) (and (<= SCREEN-MIN-X (posn-x loc) SCREEN-MAX-X)
                               (<= SCREEN-MIN-Y (posn-y loc) SCREEN-MAX-Y))))

(define (random-pair min-x max-x min-y max-y make)
  (make (+ (random (- max-x
                      min-x))
           min-x)
        (+ (random (- max-y
                      min-y))
           min-y)))

(define PLAYER1 (make-player (random-pair SCREEN-MIN-X
                                          SCREEN-MAX-X
                                          SCREEN-MIN-Y
                                          SCREEN-MAX-Y
                                          make-posn)
                             (draw-fish PLAYER-SMALL
                                        PLAYER-COLOR)
                             PLAYER-SMALL
                             #false))

;; Percentage chances that a randomly generated Enemy 
;;  is of a given size:
(define ENEMY-SMALL-CHANCE 60)
(define ENEMY-MED-CHANCE 30)
(define ENEMY-LARGE-CHANCE 10)
(check-expect (+ ENEMY-SMALL-CHANCE
                 ENEMY-MED-CHANCE
                 ENEMY-LARGE-CHANCE)
              100)

;; create-random-enemy: NatNum -> Enemy
;; Consumes:
;;  - NatNum n: the seed for the Enemy
;; Produces: an Enemy located within the bounds of the screen,
;;           and which doesn'y collide with PLAYER1
;;           with a velocity whose components are random real numbers
;;           each in [-SPEED-MAX, SPEED-MAX], with a fixed probability
;;           of being small, medium, or large in size and holding an
;;           appropriate Image

(check-satisfied (create-random-enemy 0)
                 (λ (an-enemy) (not (collide? PLAYER1 an-enemy))))
(check-satisfied (enemy-loc (create-random-enemy 0))
                 (λ (loc) (equal? loc (wrap-posn-to-screen loc))))
(check-random (enemy-pic (create-random-enemy 0))
              (local [(define TEST (create-random-enemy 0))]
                (draw-rotated-enemy-pic (enemy-vel TEST)
                                        (enemy-size TEST))))              
(check-satisfied (enemy-size (create-random-enemy 0))
                 (λ (size) (or (= size ENEMY-SMALL)
                               (= size ENEMY-MED)
                               (= size ENEMY-LARGE))))
(check-satisfied (enemy-vel (create-random-enemy 0))
                 (λ (vel)
                   (and (<= (* -1 SPEED-MAX) (velocity-x vel) SPEED-MAX)
                        (<= (* -1 SPEED-MAX) (velocity-y vel) SPEED-MAX))))

(define (create-random-enemy n)
  (local [(define rand (random 100))
          (define SIZE
            (cond [(< rand ENEMY-SMALL-CHANCE) ENEMY-SMALL]
                  [(< rand (+ ENEMY-SMALL-CHANCE
                              ENEMY-MED-CHANCE)) ENEMY-MED]
                  [else ENEMY-LARGE]))
          (define VEL (random-pair (* -1 SPEED-MAX)
                                     SPEED-MAX
                                     (* -1 SPEED-MAX)
                                     SPEED-MAX
                                     make-velocity))
          (define CREATED-ENEMY
            (make-enemy (random-pair SCREEN-MIN-X
                                     SCREEN-MAX-X
                                     SCREEN-MIN-Y
                                     SCREEN-MAX-Y
                                     make-posn)
                        (draw-rotated-enemy-pic VEL
                                                SIZE)
                        SIZE
                        VEL))]
    (if (not (collide? PLAYER1 CREATED-ENEMY))
        CREATED-ENEMY
        (create-random-enemy n))))

;; Initial state of the fish game:
(define START (make-fish-world PLAYER1
                               (build-list 25 create-random-enemy)
                               0))

;;----------Kinetics:

;; re-apear: Player -> Player
;; Consumes:
;; Player a-player: the inputed Player
;; Produces: a new Player on the other side of the screen if the a-player's 
;;           position exceeds one of the screen's limits
(check-expect (re-appear (make-player (make-posn 50 (+ SCREEN-MAX-Y 1))
                                      (draw-fish PLAYER-SMALL "blue")
                                      PLAYER-SMALL
                                      #false))
              (make-player (make-posn 50 SCREEN-MIN-Y)
                           (draw-fish PLAYER-SMALL "blue")
                           PLAYER-SMALL
                           #false))
(check-expect (re-appear (make-player (make-posn 50 (+ SCREEN-MIN-Y -1))
                                      (draw-fish PLAYER-SMALL "blue")
                                      PLAYER-SMALL
                                      #false))
              (make-player (make-posn 50 SCREEN-MAX-Y)
                           (draw-fish PLAYER-SMALL "blue")
                           PLAYER-SMALL
                           #false))
(check-expect (re-appear (make-player (make-posn (+ SCREEN-MAX-X 1) 50)
                                      (draw-fish PLAYER-SMALL "blue")
                                      PLAYER-SMALL
                                      #false))
              (make-player (make-posn SCREEN-MIN-X 50)
                           (draw-fish PLAYER-SMALL "blue")
                           PLAYER-SMALL
                           #false))
(check-expect (re-appear (make-player (make-posn (+ SCREEN-MIN-X -1) 50)
                                      (draw-fish PLAYER-SMALL "blue")
                                      PLAYER-SMALL
                                      #false))
              (make-player (make-posn SCREEN-MAX-X 50)
                           (draw-fish PLAYER-SMALL "blue")
                           PLAYER-SMALL
                           #false))
(check-expect (re-appear (make-player (make-posn 50 50)
                                      (draw-fish PLAYER-SMALL "blue")
                                      PLAYER-SMALL
                                      #false))
              (make-player (make-posn 50 50)
                           (draw-fish PLAYER-SMALL "blue")
                           PLAYER-SMALL
                           #false))

(define (re-appear a-player)
  (if (or (> (posn-y (player-loc a-player)) SCREEN-MAX-Y)
          (< (posn-y (player-loc a-player)) SCREEN-MIN-Y)
          (> (posn-x (player-loc a-player)) SCREEN-MAX-X)
          (< (posn-x (player-loc a-player)) SCREEN-MIN-X))
      (make-player (wrap-posn-to-screen (player-loc a-player))
                   (player-pic a-player)
                   (player-size a-player)
                   (player-eaten? a-player))
      a-player))

;; move-player: KeyEvent Player-> Posn
;; Consumes:
;; Player   a-player: the inputed Player
;; KeyEvent      key: the inputed KeyEvent
;; Produces a new Player, moved and rotated based on the KeyEvent
(check-expect (move-player (make-player (make-posn 50 50)
                                        (draw-fish PLAYER-SMALL "blue")
                                        PLAYER-SMALL
                                        #false)
                           "down")
              (make-posn 50 (- 50 PLAYER-MOVE)))
(check-expect (move-player (make-player (make-posn 50 50)
                                        (draw-fish PLAYER-SMALL "blue")
                                        PLAYER-SMALL
                                        #false)
                           "up")
              (make-posn 50 (+ 50 PLAYER-MOVE)))
(check-expect (move-player (make-player (make-posn 50 50)
                                        (draw-fish PLAYER-SMALL "blue")
                                        PLAYER-SMALL
                                        #false)
                           "right")
              (make-posn (- 50 PLAYER-MOVE) 50))
(check-expect (move-player (make-player (make-posn 50 50)
                                        (draw-fish PLAYER-SMALL "blue")
                                        PLAYER-SMALL
                                        #false)
                           "left")
              (make-posn (+ 50 PLAYER-MOVE) 50))

(define (move-player a-player key)
  (make-posn (+ (posn-x (player-loc a-player))
                (cond [(key=? key "right") (- 0 PLAYER-MOVE)]
                      [(key=? key "left") PLAYER-MOVE]
                      [else 0]))
             (+ (posn-y (player-loc a-player))
                (cond [(key=? key "down") (- 0 PLAYER-MOVE)]
                      [(key=? key "up") PLAYER-MOVE]
                      [else 0]))))

;; player-key-handler: Player KeyEvent -> Player
;; Consumes:
;; Player   a-player: the curent Player
;; KeyEvent      key: the inputed KeyEvent
;; Produces a new Player, moved and rotated based on the KeyEvent

(check-expect (player-key-handler (make-player (make-posn 50 50)
                                               (player-pic PLAYER1)
                                               PLAYER-SMALL
                                               #false)
                                  "up")
              (make-player (make-posn 50 (+ 50 PLAYER-MOVE))
                           (rotate-player PLAYER1
                                          "up")
                           PLAYER-SMALL
                           #false))
(check-expect (player-key-handler (make-player (make-posn 50 SCREEN-MIN-Y)
                                               (player-pic PLAYER1)
                                               PLAYER-SMALL
                                               #false)
                                  "down")
              (make-player (make-posn 50 (wrap-coordinate (- SCREEN-MIN-Y
                                                             PLAYER-MOVE)
                                                          SCREEN-MIN-Y
                                                          SCREEN-MAX-Y))
                           (rotate-player PLAYER1
                                          "down")
                           PLAYER-SMALL
                           #false))
(check-expect (player-key-handler (make-player (make-posn 50 SCREEN-MIN-Y)
                                               (player-pic PLAYER1)
                                               PLAYER-SMALL
                                               #false)
                                  "p")
              (make-player (make-posn 50 SCREEN-MIN-Y)
                           (player-pic PLAYER1)
                           PLAYER-SMALL
                           #false))

(define (player-key-handler a-player key)
  (re-appear (make-player (move-player a-player key)
                          (if (arrow-key? key)
                              (rotate-player a-player
                                             key)
                              (player-pic a-player))
                          (player-size a-player)
                          (player-eaten? a-player))))

;; key-handler: FishWorld KeyEvent -> FishWorld
;; Consumes:
;;  - FishWorld  fw: the curent FishWorld
;;  - KeyEvent  key: the inputed KeyEvent
;; Produces a new FishWorld based on the KeyEvent

(check-expect (key-handler TEST-WORLD "left")
              (make-fish-world (player-key-handler
                                (fish-world-player TEST-WORLD)
                                "left")
                               (fish-world-enemies TEST-WORLD)
                               0))

(define (key-handler fw key)
  (make-fish-world (player-key-handler (fish-world-player fw)
                                       key)
                   (fish-world-enemies fw)
                   (fish-world-score fw)))

;; The proportion of edible Enemies which a Player needs to consume
;;  in order to advance to the next size:
(define THRESHOLD-PROPORTION 2/3)
;; The scores which need to be attained in order for a Player
;;  to advance to a given size:
(define PLAYER-MED-THRESHOLD
  (* (length (filter (λ (an-enemy) (= (enemy-size an-enemy) ENEMY-SMALL))
                     (fish-world-enemies START)))
     THRESHOLD-PROPORTION))
(define PLAYER-LARGE-THRESHOLD
  (+ PLAYER-MED-THRESHOLD
     (* (length (filter (λ (an-enemy) (= (enemy-size an-enemy) ENEMY-MED))
                        (fish-world-enemies START)))
        2
        THRESHOLD-PROPORTION)))
;; tick-player: Player Int Boolean -> Player
;; Consumes:
;;  - Player    a-player: the Player whose state is to be updated
;;  - Int          score: the Player's current score
;;  - Boolean eaten?: whether or not the Player has been eaten
;; Produces: a copy of a-player whose size and pic fields
;;           are updated if score has passed a threshold and
;;           these fields have yet to be adjusted accordingly,
;;           and sets a-player's eaten? field to match eaten?
(check-expect (tick-player PLAYER1 0 #false) PLAYER1)
(check-expect (tick-player PLAYER1
                           (ceiling PLAYER-MED-THRESHOLD)
                           #false)
              (make-player (player-loc PLAYER1)
                           (draw-fish PLAYER-MED
                                      PLAYER-COLOR)
                           PLAYER-MED
                           #false))
(check-expect (tick-player PLAYER1
                           (ceiling PLAYER-LARGE-THRESHOLD)
                           #true)
              (make-player (player-loc PLAYER1)
                           (draw-fish PLAYER-LARGE
                                      PLAYER-COLOR)
                           PLAYER-LARGE
                           #true))

(define (tick-player a-player score eaten?)
  (cond [(and (>= score PLAYER-LARGE-THRESHOLD)
              (< (player-size a-player) PLAYER-LARGE))
         (make-player (player-loc a-player)
                      (draw-fish PLAYER-LARGE
                                 PLAYER-COLOR)
                      PLAYER-LARGE
                      eaten?)]
        [(and (>= score PLAYER-MED-THRESHOLD)
              (< (player-size a-player) PLAYER-MED))
         (make-player (player-loc a-player)
                      (draw-fish PLAYER-MED
                                 PLAYER-COLOR)
                      PLAYER-MED
                      eaten?)]
        [else
         (make-player (player-loc a-player)
                      (player-pic a-player)
                      (player-size a-player)
                      eaten?)]))

;; Probability (in percent) that a given Enemy's velocity will be
;;  randomized in a given tick:
(define ENEMY-CHANGE-CHANCE 5)
;; tick-enemy: Enemy -> Enemy
;; Consumes:
;;  - Enemy an-enemy: the Enemy to be updated for the next tick
;; Produces: a new Enemy with an adjusted position and a
;;           vel with a chance, ENEMY-CHANGE-CHANCE, of being 
;;           randomized so that its components are equally likely 
;;           to be any Int within [-SPEED-MAX, SPEED-MAX]

(check-expect (enemy-loc (tick-enemy ENEMY1))
              (wrap-posn-to-screen
               (make-posn
                (+ (posn-x (enemy-loc ENEMY1))
                   (velocity-x (enemy-vel ENEMY1)))
                (+ (posn-y (enemy-loc ENEMY1))
                   (velocity-y (enemy-vel ENEMY1))))))
(check-random (enemy-pic (tick-enemy ENEMY1))
              (local [(define RAND (random 100))
                      (define VEL
                        (if (< RAND ENEMY-CHANGE-CHANCE)
                            (random-pair (* -1 SPEED-MAX)
                                         SPEED-MAX
                                         (* -1 SPEED-MAX)
                                         SPEED-MAX
                                         make-velocity)
                            (enemy-vel ENEMY1)))]
                (if (< RAND ENEMY-CHANGE-CHANCE)
                    (draw-rotated-enemy-pic VEL
                                            (enemy-size ENEMY1))
                    (enemy-pic ENEMY1))))
(check-expect (enemy-size (tick-enemy ENEMY1))
              (enemy-size ENEMY1))
(check-satisfied (enemy-vel (tick-enemy ENEMY1))
                 (λ (vel) (and (<= (* -1 SPEED-MAX)
                                   (velocity-x vel)
                                   SPEED-MAX)
                               (<= (* -1 SPEED-MAX)
                                   (velocity-y vel)
                                   SPEED-MAX))))

(define (tick-enemy an-enemy)
  (local [(define (posn-changer loc vel)
            (wrap-posn-to-screen
             (make-posn (+ (posn-x loc) (velocity-x vel))
                        (+ (posn-y loc) (velocity-y vel)))))
          (define RAND (random 100))
          (define VEL
            (if (< RAND ENEMY-CHANGE-CHANCE)
                (random-pair (* -1 SPEED-MAX)
                             SPEED-MAX
                             (* -1 SPEED-MAX)
                             SPEED-MAX
                             make-velocity)
                (enemy-vel an-enemy)))]
    (make-enemy (posn-changer (enemy-loc an-enemy)
                              (enemy-vel an-enemy))
                (if (< RAND ENEMY-CHANGE-CHANCE)
                    (draw-rotated-enemy-pic VEL
                                            (enemy-size an-enemy))
                    (enemy-pic an-enemy))
                (enemy-size an-enemy)
                VEL)))

;; tick-handler: FishWorld -> FishWorld
;; Consumes:
;;  - FishWorld fw: the curent FishWorld
;; Produces: a new FishWorld by ticking the player and all enemies
;;           with player-tick and enemy-tick, respectively

(check-random (fish-world-player (tick-handler TEST-WORLD))
              (tick-player
               (fish-world-player TEST-WORLD)
               (fish-world-score TEST-WORLD)
               (handle-player-eating
                (map tick-enemy (fish-world-enemies TEST-WORLD))
                (fish-world-player TEST-WORLD))))
(check-random (fish-world-enemies (tick-handler TEST-WORLD))
              (handle-enemy-eating
               (map tick-enemy (fish-world-enemies TEST-WORLD))
               (fish-world-player TEST-WORLD)))
(check-random (fish-world-score (tick-handler TEST-WORLD))
              (accumulate-score (fish-world-score TEST-WORLD)
                                (fish-world-player TEST-WORLD)
                                (map tick-enemy
                                     (fish-world-enemies TEST-WORLD))))

(define (tick-handler fw)
  (local [(define NEW-LOE (map tick-enemy (fish-world-enemies fw)))]
    (make-fish-world
     (tick-player (fish-world-player fw)
                  (fish-world-score fw)
                  (handle-player-eating NEW-LOE (fish-world-player fw)))
     (handle-enemy-eating
      NEW-LOE
      (fish-world-player fw))
     (accumulate-score (fish-world-score fw)
                       (fish-world-player fw)
                       NEW-LOE))))

;;handle-enemy-eating : [ListOf Enemy] Player -> [ListOf Enemy]
;; Consumes:
;;  - [ListOf Enemy] aloe: contains all Enemies that might have
;;                         collided with player
;;  - Player       player: the Player that the Enemies in aloe
;;                         may have collied with
;; Produces: a [ListOf Enemy] containing all Enemies in aloe which
;;           player doesn't eat
(check-expect (handle-enemy-eating (list ENEMY@CENT)
                                   PLAYER@CENT)
              (local [(define COLLIDED-FISH
                        (filter (λ (an-enemy)
                                  (collide? PLAYER@CENT an-enemy))
                                (list ENEMY@CENT)))
                      (define (filter-size aloe a-player)
                        (filter (λ (an-enemy)
                                  (< (enemy-size an-enemy)
                                     (player-size a-player)))
                                aloe))]
                (if (empty? COLLIDED-FISH)
                    (fish-world-enemies TEST-WORLD)
                    (filter
                     (λ (an-enemy)
                       (not (member an-enemy
                                    (filter-size COLLIDED-FISH
                                                 PLAYER@CENT))))
                     (list ENEMY@CENT)))))

(check-expect (handle-enemy-eating (list ENEMY1)
                                   PLAYER@CENT)
              (local [(define COLLIDED-FISH
                        (filter (λ (an-enemy) (collide? PLAYER@CENT
                                                            an-enemy))
                                (list ENEMY1)))
                      (define (filter-size aloe a-player)
                        (filter (λ (an-enemy) (< (enemy-size an-enemy)
                                               (player-size a-player)))
                                aloe))]
                (if (empty? COLLIDED-FISH)
                    (list ENEMY1)
                    (filter
                     (λ (an-enemy)
                       (not (member e
                                    (filter-size COLLIDED-FISH PLAYER@CENT))))
                     (list ENEMY1)))))

(define (handle-enemy-eating aloe a-player)
    (filter (λ (an-enemy) (not (and (collide? a-player an-enemy)
                             (< (enemy-size an-enemy)
                                (player-size a-player)))))
            aloe))

;; handle-player-eating : [ListOf Enemy] Player -> Boolean
;; Consumes:
;;  - [ListOf Enemy]     aloe: the [ListOf Enemy] in the current FishWorld 
;;  - Player         a-player: the current Player
;; Produces: whether a-player has colided with an Enemy larger than it

(check-expect (handle-player-eating (list ENEMY@CENT) PLAYER@CENT) #true)
(check-expect (handle-player-eating (list ENEMY1) PLAYER@CENT-RIGHT) #false)

(define (handle-player-eating aloe a-player)
  (ormap (λ (an-enemy) (and (> (enemy-size an-enemy) (player-size a-player))
                            (collide? a-player an-enemy))) 
         aloe))

;;---------Scoring:-------

;; A PointVal is one of the following:
(define SMALL-POINTS 1)
(define MED-POINTS 2)
(define LARGE-POINTS 3)

(define SCORING-PLAYER1 (make-player (make-posn 25 25)
                                     (draw-fish PLAYER-SMALL
                                                PLAYER-COLOR)
                                     PLAYER-SMALL
                                     #false))

(define SCORING-PLAYER2 (make-player (make-posn 25 25)
                                     (draw-fish PLAYER-LARGE
                                                PLAYER-COLOR)
                                     PLAYER-LARGE
                                     #false))

(define SCORING-ENEMY1 (make-enemy (make-posn 25 25)
                                   (draw-fish ENEMY-LARGE
                                              ENEMY-COLOR)
                                   ENEMY-LARGE
                                   (make-velocity 3 4)))

(define SCORING-ENEMY2 (make-enemy (make-posn 25 25)
                                   (draw-fish ENEMY-MED
                                              ENEMY-COLOR)
                                   ENEMY-MED
                                   (make-velocity 3 4)))

(define SCORING-ENEMY3 (make-enemy (make-posn 25 25)
                                   (draw-fish ENEMY-SMALL
                                              ENEMY-COLOR)
                                   ENEMY-SMALL
                                   (make-velocity 3 4)))

(define SCORING-ENEMY4 (make-enemy (make-posn 100 100)
                                   (draw-fish ENEMY-SMALL
                                              ENEMY-COLOR)
                                   ENEMY-SMALL
                                   (make-velocity 3 4)))
(define SCORING-FW1 (make-fish-world
                     SCORING-PLAYER1
                     (list SCORING-ENEMY1 SCORING-ENEMY2)
                     0))


(define SCORING-FW2 (make-fish-world
                     SCORING-PLAYER2
                     (list SCORING-ENEMY1 SCORING-ENEMY2 SCORING-ENEMY4)
                     0))

;; accumulate-score: Player [ListOf Enemies] -> PosInt
;; Consumes
;;  - Player           a-player: the existing Player
;;  - [ListOf Enemies]  enemies: an existing [ListOf Enemies]
;; Produces: a score for a-player based on any collisions with enemies
(check-expect (accumulate-score 5 (fish-world-player SCORING-FW1)
                                (fish-world-enemies SCORING-FW1))
              5)
(check-expect (accumulate-score 0 (fish-world-player SCORING-FW1)
                                (fish-world-enemies SCORING-FW1))
              0)
(check-expect (accumulate-score 0 (fish-world-player SCORING-FW2)
                                (fish-world-enemies SCORING-FW2))
              5)


(define (accumulate-score  score a-player enemies)
  (local [(define COLLIDED-FISH (filter (λ (an-enemy)
                                          (collide? a-player an-enemy))
                                        enemies))]
    (cond
      [(empty? COLLIDED-FISH) score]
      [(cons? COLLIDED-FISH) 
       (+ (get-points a-player (first COLLIDED-FISH)) 
          (accumulate-score score a-player (rest COLLIDED-FISH)))])))


;; get-points: PosInt Player Enemy -> PosInt
;; Consumes:
;;  - PosInt    score: the curent score
;;  - Player a-player: the current Player
;;  - Enemy  an-enemy: an Enemy that has collided with a-player
;; Produces: a new score value, the result of adding the point value 
;;           of an-enemy, if it is smaller than a-player to score
(check-expect (get-points SCORING-PLAYER1 SCORING-ENEMY1) 0)
(check-expect (get-points SCORING-PLAYER2 SCORING-ENEMY1) 3)
(check-expect (get-points SCORING-PLAYER2 SCORING-ENEMY2) 2)
(check-expect (get-points SCORING-PLAYER2 SCORING-ENEMY3) 1)
(check-expect (get-points SCORING-PLAYER1 SCORING-ENEMY4) 1)

(define (get-points a-player an-enemy) 
  (cond
    [(> (enemy-size an-enemy) (player-size a-player)) 0]
    [(= (enemy-size an-enemy) ENEMY-SMALL) SMALL-POINTS]
    [(= (enemy-size an-enemy) ENEMY-MED) MED-POINTS]
    [(= (enemy-size an-enemy) ENEMY-LARGE) LARGE-POINTS]))


;;---------Doomstick:----------

(define EATEN-PLAYER1 (make-player (player-loc PLAYER1)
                                   (player-pic PLAYER1)
                                   (player-size PLAYER1)
                                   #true))

;; player-or-all-enemies-eaten?: FishWorld -> Boolean
;; Consumes:
;;  - FishWorld fw: the current state of the game
;; Produces: whether or not either the Player or all of the enemies
;;           in fw are gone
(check-expect (player-or-all-enemies-eaten?
               (make-fish-world EATEN-PLAYER1
                                LOE1
                                0))
              #true)
(check-expect (player-or-all-enemies-eaten?
               (make-fish-world PLAYER1
                                '()
                                10))
              #true)
(check-expect (player-or-all-enemies-eaten? TEST-WORLD)
              #false)

(define (player-or-all-enemies-eaten? fw)
  (or (player-eaten? (fish-world-player fw))
      (empty? (fish-world-enemies fw))))

(define LOST-TEXT "Game Over")
(define WON-TEXT "You win!")
;; render-doomstick: FishWorld -> Image
;;  Consumes:
;;   - FishWorld fw: the final state of the simulation
;;  Produces: An Image containing the Player or all remaining Enemies
;;            at appropriate locations, the cuurent score in the upper
;;            right corner of the screen, and either WON-TEXT or
;;            LOST-TEXT in the center of the screen
(check-expect (render-doomstick
               (make-fish-world EATEN-PLAYER1 
                                LOE1
                                0))
              (foldr render/overlay/crop
                     BACKGROUND
                     (append (list (make-rendering (text LOST-TEXT
                                                         SCORE-SIZE
                                                         SCORE-COLOR)
                                                   (make-posn 0 0)))
                             (map enemy->rendering
                                  LOE1))))
(check-expect (render-doomstick
               (make-fish-world PLAYER1
                                '()
                                10))
              (foldr render/overlay/crop
                     BACKGROUND
                     (list (score->rendering 10)
                           (player->rendering PLAYER1)
                           (make-rendering (text WON-TEXT
                                                 SCORE-SIZE
                                                 SCORE-COLOR)
                                           (make-posn 0 0)))))

(define (render-doomstick fw)
  (cond [(player-eaten? (fish-world-player fw))
         (foldr render/overlay/crop
                BACKGROUND
                (append (list (make-rendering (text LOST-TEXT
                                                    SCORE-SIZE
                                                    SCORE-COLOR)
                                              (make-posn 0 0)))
                        (map enemy->rendering
                             (fish-world-enemies fw))))]
        [(empty? (fish-world-enemies fw))
         (foldr render/overlay/crop
                BACKGROUND
                (list (score->rendering (fish-world-score fw))
                      (player->rendering (fish-world-player fw))
                      (make-rendering (text WON-TEXT
                                            SCORE-SIZE
                                            SCORE-COLOR)
                                      (make-posn 0 0))))]))

;;---------Main:----------

;; main: FishWorld -> FishWorld
;;  Consumes:
;;   - FishWorld fw: the curent state of the simulation
;;  Produces: the next state of the fw

(define (main fw)
  (big-bang fw
            [on-key key-handler]
            [to-draw render]
            [on-tick  tick-handler]
            [stop-when player-or-all-enemies-eaten?
                       render-doomstick]))

(main START)