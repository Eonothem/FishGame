;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname fish) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; Partners:
;; - Thomas Kitson
;; - Connor Onweller
;; - James skripchuck

(require 2htdp/image)
(require 2htdp/universe)

;;----------WELCOME TO FISH WORLD----------

;;----------Basic Types:

;; A Color is a String, one of:
;;  - "red"
;;  - "orange"
;;  - "green"
;;  - "blue"
;;  - or any other String allowed in color-database<%>

;; An X-Place is a String, one of;
;;  - "left"
;;  - "center"
;;  - "right"

;; An Y-Place is a String, one of;
;;  - "top"
;;  - "center"
;;  - "bottom"

;; A Place is either a X-place or a Y-Place

;; posn-x-place: Posn -> X-Place
;;  Consumes:
;;   - Posn loc: a Posn representing a location on the screen
;;  Produces: the X-Place corresponding to the quadrant wherein
;;            loc is located, if any

(check-expect (posn-x-place (player-loc PLAYER@BOT-LEFT))
              "left")
(check-expect (posn-x-place (player-loc PLAYER@CENT))
              "center")
(check-expect (posn-x-place (player-loc PLAYER@TOP-RIGHT))
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

(check-expect (posn-y-place (player-loc PLAYER@BOT-LEFT))
              "bottom")
(check-expect (posn-y-place (player-loc PLAYER@CENT))
              "center")
(check-expect (posn-y-place (player-loc PLAYER@TOP-RIGHT))
              "top")

(define (posn-y-place loc)
  (cond [(> (posn-y loc) 0) "top"]
        [(= (posn-y loc) 0) "center"]
        [(< (posn-y loc) 0) "bottom"]))

;; opposite-place: Place -> Place
;;  Consumes:
;;   - Place aplace: the Place to be inverted
;;  Produces: the Place opposite aplace

(check-expect (opposite-place "top") "bottom")
(check-expect (opposite-place "right") "left")
(check-expect (opposite-place "center") "center")
(check-expect (opposite-place "left") "right")
(check-expect (opposite-place "bottom") "top")

(define (opposite-place aplace)
  (cond [(string=? "top" aplace) "bottom"]
        [(string=? "right" aplace) "left"]
        [(string=? "center" aplace) "center"]
        [(string=? "left" aplace) "right"]
        [(string=? "bottom" aplace) "top"]))

;; An ArrowKey is a EventKey, one of:
;;  - "right"
;;  - "up"
;;  - "left"
;;  - "down"

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

;;----------Background Constants:
(define BACKGROUND-WIDTH 750)
(define BACKGROUND-HEIGHT 500)
(define BACKGROUND-COLOR "blue")
(define BACKGROUND (rectangle BACKGROUND-WIDTH
                              BACKGROUND-HEIGHT
                              "solid"
                              BACKGROUND-COLOR))

;;----------Screen Constants:
(define SCREEN-MIN-X (/ BACKGROUND-WIDTH -2))
(define SCREEN-MAX-X (/ BACKGROUND-WIDTH 2))
(define SCREEN-MIN-Y (/ BACKGROUND-HEIGHT -2))
(define SCREEN-MAX-Y (/ BACKGROUND-HEIGHT 2))

;; random-pair: Int Int Int Int (Number Number -> X) -> X
;;  Consumes:
;;   - Int min-x: the minimum value for the x-coordinate
;;   - Int max-x: the maximum value for the x-coordinate
;;   - Int min-y: the minimum value for the y-coordinate
;;   - Int max-y: the maximum value for the y-coordinate
;;   - (Number Number -> X) make: a constuctor which consumes
;;                                two Numbers to instantiate
;;                                a structure
;;  Produces: a Posn with x as an Int within [min-x, max-x]
;;   and y as an Int within [min-y, max-y]

(check-random (random-pair SCREEN-MIN-X
                           SCREEN-MAX-X
                           SCREEN-MIN-Y
                           SCREEN-MAX-Y
                           make-posn)
              (make-posn (+ (random (- SCREEN-MAX-X
                                       SCREEN-MIN-X))
                            SCREEN-MIN-X)
                         (+ (random (- SCREEN-MAX-Y
                                       SCREEN-MIN-Y))
                            SCREEN-MIN-Y)))

(define (random-pair min-x max-x min-y max-y make)
  (make (+ (random (- max-x
                      min-x))
           min-x)
        (+ (random (- max-y
                      min-y))
           min-y)))

;----------Rendering----------

;; draw-fish: PosInt Color -> Image
;;  Consumes:
;;   - PosInt size:  the height of the fish to be drawn
;;   - Color  color: the color the fish is to be drawn in
;;  Produces: an image depicting a fish
(define FISH-HEIGHT/WIDTH 9/5)

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

;; rotate-player: ArrowKey Player -> Image
;;  Consunes:
;;   - ArrowKey k: the key pressed by the user
;;   - Player   p: the current state of the player
;;  Produces: an image representing p,
;;   moving in the direction indicated by k

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

(define (rotate-player p k)
  (rotate (cond [(key=? "left" k)  0]
                [(key=? "down" k)    90]
                [(key=? "right" k) 180]
                [(key=? "up" k) 270])
          (draw-fish (player-size p)
                     PLAYER-COLOR)))

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

;; a ListOfRenderings [LoR] is one of:
;;  - '()
;;  - (cons Rendering LoR)

;;Examples:
(define LOR-0 '())
(define LOR-1 (cons REND-1 '()))

;; Template:
#;
(define (lor-fn lor)
  (cond [(empty? lor) ...]
        [(cons? lor)
         ... (rendering-fn (first lor))
         ... (lor-fn (rest lor))]))

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
                                    (ceiling (/ (image-width score-pic) 2)))
                                 (- SCREEN-MAX-Y
                                    (ceiling (/ (image-height score-pic) 2)))))))

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
;;   - Player p: the Player to be rendered
;;  Produces: A Rendering representing how p should be rendered;
;;            holds p's pic and loc

(check-expect (player->rendering PLAYER1)
              (make-rendering (player-pic PLAYER1)
                                    (player-loc PLAYER1)))

(define (player->rendering p)
  (make-rendering (player-pic p)
                        (player-loc p)))

;; enemy->rendering: Enemy -> Rendering
;;  Consumes:
;;   - Enemy e: the Enemy to be rendered
;;  Produces: A Rendering representing how e should be rendered;
;;            holds e's pic and loc

(check-expect (enemy->rendering SHARK)
              (make-rendering (enemy-pic SHARK)
                                    (enemy-loc SHARK)))

(define (enemy->rendering e)
  (make-rendering (enemy-pic e)
                        (enemy-loc e)))

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
;;   Enemies at appropriate locations, along with the current
;;   score in the upper right corner of the screen

(check-expect (render START)
              (foldr render/overlay/crop
                     BACKGROUND
                     (append
                      (list (score->rendering (fish-world-score START))
                            (player->rendering (fish-world-player START)))
                      (map enemy->rendering
                           (fish-world-enemies START)))))

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
                 #|(define cropped-quad-y
                   (crop/align "center"
                               "bottom"
                               (image-width BACKGROUND)
                               (- (image-height comp)
                                  (image-height BACKGROUND))
                               comp))|#
                 (define uncropped-quad
                   (crop/align "center"
                               "top"
                               (image-width BACKGROUND)
                               (image-height BACKGROUND)
                               comp))]
                 ;(overlay/align
                 ;"right"
                 ;"top"
                 ;cropped-quad-y
                 uncropped-quad))
                 ;BACKGROUND)))
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
                                   BACKGROUND))
                 #|(define cropped-quad-x
                   (crop/align
                    "right"
                    "center"
                    (- (image-width comp)
                       (image-width BACKGROUND))
                    (image-height BACKGROUND)
                    comp))|#
                 (define uncropped-quad
                   (crop/align "left"
                               "center"
                               (image-width BACKGROUND)
                               (image-height BACKGROUND)
                               comp))]
                 ;(overlay/align
                 ;"left"
                 ;"bottom"
                 ;cropped-quad-x
                 uncropped-quad))
                 ;BACKGROUND)))

(define (crop-to-background comp rend bkgnd)
  (local
    [#|(define cropped-quad-xy
       (crop/align (posn-x-place (rendering-loc rend))
                   (posn-y-place (rendering-loc rend))
                   (- (image-width comp)
                      (image-width bkgnd))
                   (- (image-height comp)
                      (image-height bkgnd))
                   comp))
     (define cropped-quad-x
       (crop/align
        (posn-x-place (rendering-loc rend))
        (opposite-place (posn-y-place (rendering-loc rend)))
        (- (image-width comp)
           (image-width bkgnd))
        (image-height bkgnd)
        comp))
     (define cropped-quad-y
       (crop/align (opposite-place (posn-x-place (rendering-loc rend)))
                   (posn-y-place (rendering-loc rend))
                   (image-width bkgnd)
                   (- (image-height comp)
                      (image-height bkgnd))
                   comp))|#
     (define uncropped-quad
       (crop/align (opposite-place (posn-x-place (rendering-loc rend)))
                   (opposite-place (posn-y-place (rendering-loc rend)))
                   (image-width bkgnd)
                   (image-height bkgnd)
                   comp))]
    #|(overlay/align
     (opposite-place (posn-x-place (rendering-loc rend)))
     (opposite-place (posn-y-place (rendering-loc rend)))
     cropped-quad-xy
     (overlay/align
      (opposite-place (posn-x-place (rendering-loc rend)))
      (posn-y-place (rendering-loc rend))
      cropped-quad-x
      (overlay/align
       (posn-x-place (rendering-loc rend))
       (opposite-place (posn-y-place (rendering-loc rend)))
       cropped-quad-y|#
       (overlay/align
        (posn-x-place (rendering-loc rend))
        (posn-y-place (rendering-loc rend))
        uncropped-quad
        bkgnd)));)))

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

;;----------Player----------

;; A PlayerSize is a PosInt, one of:
     (define PLAYER-SMALL 16)
     (define PLAYER-MED 26)
     (define PLAYER-LARGE 36)

(define-struct player [loc pic size])
;; A Player is a (make-player loc pic size)
;;  Interpertation:
;;   - Posn        loc: the (x,y) position of the Player,
;;                      in pixels, using the center of the screen as
;;                      the origin and center of the player as the point
;;   - Image       pic: a graphic representation of the player's fish
;;   - PlayerSize size: the current size of the player, in pixels tall

(define PLAYER-MOVE 10)
(define PLAYER-COLOR "green")

;;Examples:
(define PLAYER1 (make-player (random-pair SCREEN-MIN-X
                                          SCREEN-MAX-X
                                          SCREEN-MIN-Y
                                          SCREEN-MAX-Y
                                          make-posn)
                             (draw-fish PLAYER-SMALL
                                        PLAYER-COLOR)
                             PLAYER-SMALL))
(define PLAYER@TOP-LEFT (make-player (make-posn SCREEN-MAX-X
                                                SCREEN-MAX-Y)
                                     (player-pic PLAYER1)
                                     (player-size PLAYER1)))
(define PLAYER@TOP-CENT (make-player (make-posn 0
                                                SCREEN-MAX-Y)
                                     (player-pic PLAYER1)
                                     (player-size PLAYER1)))
(define PLAYER@TOP-RIGHT (make-player (make-posn SCREEN-MIN-X
                                                 SCREEN-MAX-Y)
                                      (player-pic PLAYER1)
                                      (player-size PLAYER1)))
(define PLAYER@CENT-LEFT (make-player (make-posn SCREEN-MAX-X
                                                 0)
                                      (player-pic PLAYER1)
                                      (player-size PLAYER1)))
(define PLAYER@CENT (make-player (make-posn 0
                                            0)
                                      (player-pic PLAYER1)
                                      (player-size PLAYER1)))
(define PLAYER@CENT-RIGHT (make-player (make-posn SCREEN-MIN-X
                                                  0)
                                       (player-pic PLAYER1)
                                       (player-size PLAYER1)))
(define PLAYER@BOT-LEFT (make-player (make-posn SCREEN-MAX-X
                                                SCREEN-MIN-Y)
                                     (player-pic PLAYER1)
                                     (player-size PLAYER1)))
(define PLAYER@BOT-CENT (make-player (make-posn 0
                                                SCREEN-MIN-Y)
                                     (player-pic PLAYER1)
                                     (player-size PLAYER1)))
(define PLAYER@BOT-RIGHT (make-player (make-posn SCREEN-MIN-X
                                                 SCREEN-MIN-Y)
                                      (player-pic PLAYER1)
                                      (player-size PLAYER1)))

;; Template:
#; (define (player-fn p)
     ... (posn-fn (player-loc p))
     ... (player-pic p)
     ... (player-size p)
     ... (player-score p))

;;----------Enemy----------

;; An EnemySize is a PosInt, one of the following:
  (define ENEMY-SMALL 10)
  (define ENEMY-MED 20)
  (define ENEMY-LARGE 30)

(define-struct velvect [vx vy])
;; A VelVect is a structure (make-velvect vx vy)
;; Interpretation:
;; Number vx: the x velocity of the VelVect
;; Number vy: the y velocity of the VelVect

;; Example:
(define FIVE (make-velvect 3 4))

;; Template:
#;
(define (velvect-fun v)
 ...(velvect-vx v)...
 ...(velvect-vy v)...)

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

;; Example:
(define SHARK (make-enemy (make-posn 25 25)
                          (draw-fish ENEMY-LARGE
                                     ENEMY-COLOR)
                          ENEMY-LARGE
                          FIVE))
;; Template:
#;
(define (enemy-fun e)
  ...(enemy-loc e)...
  ...(enemy-pic e)...
  ...(enemy-size e)...
  ...(enemy-vel e)...)

(define SPEED-MAX 5)

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
;;           with a velocity whose components are random real numbers
;;           each in [-SPEED-MAX, SPEED-MAX], with a fixed probability
;;           of being small, medium, or large in size and holding an
;;           appropriate Image

(check-random (create-random-enemy 0)
              (local [(define rand (random 100))
          (define SIZE
            (cond [(< rand ENEMY-SMALL-CHANCE) ENEMY-SMALL]
                  [(< rand (+ ENEMY-SMALL-CHANCE
                              ENEMY-MED-CHANCE)) ENEMY-MED]
                  [else ENEMY-MED]))]
    (make-enemy (random-pair SCREEN-MIN-X
                             SCREEN-MAX-X
                             SCREEN-MIN-Y
                             SCREEN-MAX-Y
                             make-posn)
                (draw-fish SIZE
                           ENEMY-COLOR)
                SIZE
                (random-pair (* -1 SPEED-MAX)
                             SPEED-MAX
                             (* -1 SPEED-MAX)
                             SPEED-MAX
                             make-velvect))))

(define (create-random-enemy n)
  (local [(define rand (random 100))
          (define SIZE
            (cond [(< rand ENEMY-SMALL-CHANCE) ENEMY-SMALL]
                  [(< rand (+ ENEMY-SMALL-CHANCE
                              ENEMY-MED-CHANCE)) ENEMY-MED]
                  [else ENEMY-LARGE]))]
    (make-enemy (random-pair SCREEN-MIN-X
                             SCREEN-MAX-X
                             SCREEN-MIN-Y
                             SCREEN-MAX-Y
                             make-posn)
                (draw-fish SIZE
                           ENEMY-COLOR)
                SIZE
                (random-pair (* -1 SPEED-MAX)
                             SPEED-MAX
                             (* -1 SPEED-MAX)
                             SPEED-MAX
                             make-velvect))))

;;----------ListOfEnemies----------

;; a List-of-Enemies [LOE] is either
;; -- '()
;; -- (cons Enemy LOE)

;;Examples:
(define E0 '())
(define E1 (cons SHARK E0))

;; TEMPLATE
#;
(define (loe-fun aloe)
  (cond [(empty? aloe) ...]
        [(cons? aloe)
         ... (fn-for-enemy (first aloe))
         ... (loe-fun (rest aloe))]))

;;----------FishWorld----------
(define-struct fish-world [player enemies score])
;; An FishWorld is a (make-fish-world player enemies score)
;;  Interrpertation:
;;   - Player         player: the player
;;   - ListOfEnemies enemies: the enemies currently in play
;;   - Int             score: the player's current score

;;Example:
(define START (make-fish-world PLAYER1
                               (build-list 15 create-random-enemy)
                               0))

;; Template:
#; (define (fish-world-fn fw)
     ... (player-fn (fish-world-player fw))
     ... (loe-fn (fish-world-enemies fw)))

;;----------Kinetics:

;; re-apear: Player -> Player
;; Consumes:
;; Player p: the inputed Player
;; Produces: a new Player on the other side of the screen if the players 
;;           position exceeds one of the screen's limits
(check-expect (re-appear (make-player (make-posn 50 (+ SCREEN-MAX-Y 1))
                                      (draw-fish PLAYER-SMALL "blue")
                                      PLAYER-SMALL))
              (make-player (make-posn 50 SCREEN-MIN-Y)
                           (draw-fish PLAYER-SMALL "blue")
                           PLAYER-SMALL))
(check-expect (re-appear (make-player (make-posn 50 (+ SCREEN-MIN-Y -1))
                                      (draw-fish PLAYER-SMALL "blue")
                                      PLAYER-SMALL))
              (make-player (make-posn 50 SCREEN-MAX-Y)
                           (draw-fish PLAYER-SMALL "blue")
                           PLAYER-SMALL))
(check-expect (re-appear (make-player (make-posn (+ SCREEN-MAX-X 1) 50)
                                      (draw-fish PLAYER-SMALL "blue")
                                      PLAYER-SMALL))
              (make-player (make-posn SCREEN-MIN-X 50)
                           (draw-fish PLAYER-SMALL "blue")
                           PLAYER-SMALL))
(check-expect (re-appear (make-player (make-posn (+ SCREEN-MIN-X -1) 50)
                                      (draw-fish PLAYER-SMALL "blue")
                                      PLAYER-SMALL))
              (make-player (make-posn SCREEN-MAX-X 50)
                           (draw-fish PLAYER-SMALL "blue")
                           PLAYER-SMALL))
(check-expect (re-appear (make-player (make-posn 50 50)
                                      (draw-fish PLAYER-SMALL "blue")
                                      PLAYER-SMALL))
              (make-player (make-posn 50 50)
                           (draw-fish PLAYER-SMALL "blue")
                           PLAYER-SMALL))

(define (re-appear p)
  (if (or (> (posn-y (player-loc p)) SCREEN-MAX-Y)
          (< (posn-y (player-loc p)) SCREEN-MIN-Y)
          (> (posn-x (player-loc p)) SCREEN-MAX-X)
          (< (posn-x (player-loc p)) SCREEN-MIN-X))
      (make-player (wrap-posn-to-screen (player-loc p))
                   (player-pic p)
                   (player-size p))
      p))

;;collide?: Player Enemy -> Boolean
;;Consumes
;;Player p : The given Player
;;Enemy e: The given Enemy
;;Returns true if p collides with e using a rectangular bounding box
;;Returns false if it does not
;;TODO Unit Tests
(define (collide? p e)
  (local [
          (define x1 (posn-x (player-loc p)))
          (define y1 (posn-y (player-loc p)))
          (define w1 (image-width (player-pic p)))
          (define h1 (image-height (player-pic p)))
            
          (define x2 (posn-x (enemy-loc e)))
          (define y2 (posn-y (enemy-loc e)))
          (define w2 (image-width (enemy-pic e)))
          (define h2 (image-height (enemy-pic e)))]

    (and (< x1 (+ x2 w2))
         (> (+ x1 w1) x2)
         (< y1 (+ y2 h2))
         (> (+ y1 h1) y2))))

(check-expect (collide?
               (make-player (make-posn 61 37) (draw-fish PLAYER-SMALL
                                        PLAYER-COLOR) 16)
               (make-enemy (make-posn 25 25)
                           (draw-fish ENEMY-LARGE
                                      ENEMY-COLOR)
                           30
                           (make-velvect 3 4)))
              #true)
(check-expect (collide?
               (make-player (make-posn 9 0) (draw-fish PLAYER-SMALL
                                        PLAYER-COLOR) 16)
               (make-enemy (make-posn 100 100)
                           (draw-fish ENEMY-LARGE
                                      ENEMY-COLOR)
                           30
                           (make-velvect 3 4)))
              #false)

;; move-player: KeyEvent Player-> Posn
;; Consumes:
;; Player p: the inputed Player
;; KeyEvent key: the inputed KeyEvent
;; Produces a new Player based on the KeyEvent
(check-expect (move-player (make-player (make-posn 50 50)
                                             (draw-fish PLAYER-SMALL "blue")
                                             PLAYER-SMALL)
                           "down")
              (make-posn 50 (- 50 PLAYER-MOVE)))
(check-expect (move-player (make-player (make-posn 50 50)
                                             (draw-fish PLAYER-SMALL "blue")
                                             PLAYER-SMALL)
                           "up")
              (make-posn 50 (+ 50 PLAYER-MOVE)))
(check-expect (move-player (make-player (make-posn 50 50)
                                             (draw-fish PLAYER-SMALL "blue")
                                             PLAYER-SMALL)
                           "right")
              (make-posn (- 50 PLAYER-MOVE) 50))
(check-expect (move-player (make-player (make-posn 50 50)
                                             (draw-fish PLAYER-SMALL "blue")
                                             PLAYER-SMALL)
                           "left")
              (make-posn (+ 50 PLAYER-MOVE) 50))

(define (move-player p key)
  (make-posn (+ (posn-x (player-loc p))
                      (cond [(key=? key "right") (- 0 PLAYER-MOVE)]
                            [(key=? key "left") PLAYER-MOVE]
                            [else 0]))
             (+ (posn-y (player-loc p))
                      (cond [(key=? key "down") (- 0 PLAYER-MOVE)]
                            [(key=? key "up") PLAYER-MOVE]
                            [else 0]))))

;; player-key-handler: Player KeyEvent -> Player
;; Consumes:
;; Player     p: the curent Player
;; KeyEvent key: the inputed KeyEvent
;; Produces a new Player based on the KeyEvent

(check-expect (player-key-handler (make-player (make-posn 50 50)
                                               (player-pic PLAYER1)
                                               PLAYER-SMALL)
                                  "up")
              (make-player (make-posn 50 (+ 50 PLAYER-MOVE))
                           (rotate-player PLAYER1
                                          "up")
                           PLAYER-SMALL))
(check-expect (player-key-handler (make-player (make-posn 50 SCREEN-MIN-Y)
                                               (player-pic PLAYER1)
                                               PLAYER-SMALL)
                                  "down")
              (make-player (make-posn 50 (wrap-coordinate (- SCREEN-MIN-Y
                                                             PLAYER-MOVE)
                                                          SCREEN-MIN-Y
                                                          SCREEN-MAX-Y))
                           (rotate-player PLAYER1
                                          "down")
                           PLAYER-SMALL))
(check-expect (player-key-handler (make-player (make-posn 50 SCREEN-MIN-Y)
                                               (player-pic PLAYER1)
                                               PLAYER-SMALL)
                                  "p")
              (make-player (make-posn 50 SCREEN-MIN-Y)
                           (player-pic PLAYER1)
                           PLAYER-SMALL))

(define (player-key-handler p key)
 (re-appear (make-player (move-player p key)
                         (if (arrow-key? key)
                             (rotate-player p
                                            key)
                             (player-pic p))
                         (player-size p))))

;; key-handler: FishWorld KeyEvent -> FishWorld
;; Consumes:
;;  - FishWorld fw: the curent FishWorld
;; KeyEvent    key: the inputed KeyEvent
;; Produces a new FishWorld based on the KeyEvent

(check-expect (key-handler START "left")
              (make-fish-world (player-key-handler (fish-world-player START)
                                                   "left")
                               (fish-world-enemies START)
                               0))

(define (key-handler fw key)
  (make-fish-world (player-key-handler (fish-world-player fw)
                                       key)
                   (fish-world-enemies fw)
                   0))



(define ENEMY1 (make-enemy (make-posn 25 25)
                           (draw-fish ENEMY-LARGE
                                     ENEMY-COLOR)
                           ENEMY-LARGE
                           (make-velvect 3 4)))
(define ENEMY2 (make-enemy (make-posn 3 2)
                           (draw-fish ENEMY-SMALL
                                     ENEMY-COLOR)
                           ENEMY-SMALL
                           (make-velvect 2 2)))
(define ENEMY3 (make-enemy (make-posn 0 10)
                           (draw-fish ENEMY-SMALL
                                      ENEMY-COLOR)
                           ENEMY-SMALL
                           (make-velvect 0 0)))
(define LOE1 (list ENEMY1 ENEMY2 ENEMY3))

(define PLAYER-MED-THRESHOLD 10)
(define PLAYER-LARGE-THRESHOLD 25)
;; tick-player: Player Int -> Player
;; Consumes:
;;  - Player p: the Player whose state is to be updated
;;  - Int    s: the Player's current score
;; Produces: a copy of p whose size and pic fields
;;           are updated if s has passed a threshold and
;;           these fields have yet to be adjusted accordingly
(check-expect (tick-player PLAYER1 0) PLAYER1)
(check-expect (tick-player PLAYER1 10)
              (make-player (player-loc PLAYER1)
                           (draw-fish PLAYER-MED
                                      PLAYER-COLOR)
                           PLAYER-MED))
(check-expect (tick-player PLAYER1 25)
              (make-player (player-loc PLAYER1)
                           (draw-fish PLAYER-LARGE
                                      PLAYER-COLOR)
                           PLAYER-LARGE))

(define (tick-player p s)
  (cond [(and (>= s PLAYER-LARGE-THRESHOLD)
              (< (player-size p) PLAYER-LARGE))
         (make-player (player-loc p)
                      (draw-fish PLAYER-LARGE
                                 PLAYER-COLOR)
                      PLAYER-LARGE)]
        [(and (>= s PLAYER-MED-THRESHOLD)
              (< (player-size p) PLAYER-MED))
         (make-player (player-loc p)
                      (draw-fish PLAYER-MED
                                 PLAYER-COLOR)
                      PLAYER-MED)]
        [else p]))
 
;; Probability (in percent) that a given Enemy's velevt will be
;;  randomized in a given tick:
(define ENEMY-CHANGE-CHANCE 5)
;; tick-enemy: Enemy -> Enemy
;; Consumes:
;;  - Enemy e: the Enemy to be updated for the next tick
;; Produces a new Enemy with an adjusted position and a
;;  vel with a chance, ENEMY-CHANGE-CHANCE, of being randomized
;;  so thatits components are equally likely to be any Int
;;  within [-SPEED-MAX, SPEED-MAX]

(check-random (tick-enemy ENEMY1)
              (make-enemy (make-posn 28 29)
                          (draw-fish ENEMY-LARGE
                                     ENEMY-COLOR)
                          ENEMY-LARGE
                          (if (< (random 100) ENEMY-CHANGE-CHANCE)
                              (random-pair (* -1 SPEED-MAX)
                                           SPEED-MAX
                                           (* -1 SPEED-MAX)
                                           SPEED-MAX
                                           make-velvect)
                              (enemy-vel ENEMY1))))
(check-random (tick-enemy ENEMY2)
              (make-enemy (make-posn 5 4)
                          (draw-fish ENEMY-SMALL
                                     ENEMY-COLOR)
                          ENEMY-SMALL
                          (if (< (random 100) ENEMY-CHANGE-CHANCE)
                              (random-pair (* -1 SPEED-MAX)
                                           SPEED-MAX
                                           (* -1 SPEED-MAX)
                                           SPEED-MAX
                                           make-velvect)
                              (enemy-vel ENEMY2))))

(define (tick-enemy e)
  (local [(define (posn-changer loc vel)
                         (wrap-posn-to-screen
                          (make-posn (+ (posn-x loc) (velvect-vx vel))
                                     (+ (posn-y loc) (velvect-vy vel)))))
          (define (vel-changer vel)
            (if (< (random 100) ENEMY-CHANGE-CHANCE)
                (random-pair (* -1 SPEED-MAX)
                             SPEED-MAX
                             (* -1 SPEED-MAX)
                             SPEED-MAX
                             make-velvect)
                vel))]
    (make-enemy (posn-changer (enemy-loc e)
                              (enemy-vel e))
                (enemy-pic e)
                (enemy-size e)
                (vel-changer (enemy-vel e)))))

;; tick-handler: FishWorld PosNum -> FishWorld
;; Consumes:
;;  - FishWorld fw: the curent FishWorld
;;  - PosNum    tick: the inputed tick
;; Produces a new FishWorld based on the tick


;;(check-expect (tick-handler START)
   ;           (make-fish-world (fish-world-player START)
             ;                  (moves-enemies (fish-world-enemies START))))



(define (tick-handler fw)
  (make-fish-world (tick-player (fish-world-player fw) (fish-world-score fw))
                   (handle-eating (map tick-enemy (fish-world-enemies fw))
                                  (fish-world-player fw))
                   (fish-world-score fw)))

;;handle-eating : [ListOf Enemies] Player -> [ListOf Enemies]
;;Filters out the fish that the player collides with
;;Doesn't accout for size yet
;;TODO: Unit Tests, size handling
(define (handle-eating aloe player)
  (local [
          (define COLLIDED-FISH (filter (lambda (e) (collide? player e)) aloe))
          (define (filter-size aloe player)
               (filter (lambda (e) (< (enemy-size e) (player-size player))) aloe))
          ]
    (if (empty? COLLIDED-FISH)
        aloe
        (filter (lambda (e) (not (member e (filter-size COLLIDED-FISH player)))) aloe)
      )))




;;enemy-equals?
;;I dont think we need this since i didn't know the member function worked in the case above
(define (enemy-equals? e1 e2)
  (and  (= (posn-x (enemy-loc e1)) (posn-x (enemy-loc e2)))
         (= (posn-y (enemy-loc e1)) (posn-y (enemy-loc e2)))
         (equal? (enemy-pic e1) (enemy-pic e2))
         (equal? (enemy-size e1) (enemy-size e2))
         (= (velvect-vx (enemy-vel e1)) (velvect-vx (enemy-loc e2)))
         (= (velvect-vy (enemy-vel e1)) (velvect-vy (enemy-loc e2)))))

;;remove-smaller : [ListOf Enemies] Player -> [ListOf Enemies]
;;Removes all of the fish smaller in aloe than the player
;;(define (remove-smaller aloe player)
  


;(define-struct fish-world [player enemies])


;;---------Scoring:-------

;; A PointVal is one of the following:
   (define SMALL-POINTS 1)
   (define MED-POINTS 2)
   (define LARGE-POINTS 3)


;(define-struct enemy [loc pic size vel])

(define SCORING-PLAYER1 (make-player (make-posn 25 25) (draw-fish PLAYER-SMALL
                                        PLAYER-COLOR) PLAYER-SMALL))

(define SCORING-PLAYER2 (make-player (make-posn 25 25) (draw-fish PLAYER-LARGE
                                        PLAYER-COLOR) PLAYER-LARGE))

(define SCORING-ENEMY1 (make-enemy (make-posn 25 25)
                           (draw-fish ENEMY-LARGE
                                      ENEMY-COLOR)
                           ENEMY-LARGE
                           (make-velvect 3 4)))

(define SCORING-ENEMY2 (make-enemy (make-posn 25 25)
                           (draw-fish ENEMY-MED
                                      ENEMY-COLOR)
                           ENEMY-MED
                           (make-velvect 3 4)))

(define SCORING-ENEMY3 (make-enemy (make-posn 25 25)
                           (draw-fish ENEMY-SMALL
                                      ENEMY-COLOR)
                           ENEMY-SMALL
                           (make-velvect 3 4)))

(define SCORING-ENEMY4 (make-enemy (make-posn 100 100)
                           (draw-fish ENEMY-SMALL
                                      ENEMY-COLOR)
                           ENEMY-SMALL
                           (make-velvect 3 4)))
(define SCORING-FW1 (make-fish-world
                     SCORING-PLAYER1
                     (list SCORING-ENEMY1 SCORING-ENEMY2)
                     0))


(define SCORING-FW2 (make-fish-world
                     SCORING-PLAYER2
                     (list SCORING-ENEMY1 SCORING-ENEMY2 SCORING-ENEMY4)
                     0))


#|
;; score-world: FishWord -> FishWorld
;; Consumes:
;; - FishWorld  fw: the existing FishWorld
;; Produces: a new fish world with an updated score

(check-expect (score-world SCORING-FW1) SCORING-FW1)
(check-expect (score-world SCORING-FW2)
              (make-fish-world
               (make-player (player-loc (fish-world-player SCORING-FW2))
                (player-pic (fish-world-player SCORING-FW2))
                (player-size (fish-world-player SCORING-FW2)))
               (fish-world-enemies SCORING-FW2)
               5))


(define (score-world fw)
  (make-fish-world
   (make-player (player-loc (fish-world-player fw))
                (player-pic (fish-world-player fw))
                (player-size (fish-world-player fw)))
   (fish-world-enemies fw)
   (accumulate-score (fish-world-player fw) (fish-world-enemies fw))))


;; accumulate-score: Player [ListOf Enemies] -> PosInt
;; Consumes
;;  - Player             player: the existing Player
;;  - [ListOf Enemies]  enemies: an existing [ListOf Enemies]
;; Produces: a score for player based on any collisions with enemies

(check-expect (accumulate-score (fish-world-player SCORING-FW1)
                           (fish-world-enemies SCORING-FW1))
                           0)
(check-expect (accumulate-score (fish-world-player SCORING-FW2)
                           (fish-world-enemies SCORING-FW2))
                           5)

(define (accumulate-score player enemies)
  (cond
    [(empty? enemies) 0]
    [(cons? enemies) 
   (+ (add-points (player-score player) player (first enemies))
      (accumulate-score player (rest enemies)))]))




;; add-points: PosInt Player Enemy -> PosInt
;; Consumes:
;;  - PosInt   score: the existing score
;;  - Player  player: the existing Player
;;  - Enemy    Enemy: an existing Enemy
;; Produces: a new score value depending on a possible collision

(check-expect (add-points 0 SCORING-PLAYER1 SCORING-ENEMY1) 0)
(check-expect (add-points 2 SCORING-PLAYER2 SCORING-ENEMY1) 5)
(check-expect (add-points 0 SCORING-PLAYER2 SCORING-ENEMY2) 2)
(check-expect (add-points 0 SCORING-PLAYER2 SCORING-ENEMY3) 1)
(check-expect (add-points 3 SCORING-PLAYER1 SCORING-ENEMY4) 3)

(define (add-points score player enemy)
  (if (collide? player enemy) (+ score
      (cond
        [(> (enemy-size enemy) (player-size player)) 0]
        [(= (enemy-size enemy) ENEMY-SMALL) SMALL-POINTS]
        [(= (enemy-size enemy) ENEMY-MED) MED-POINTS]
        [(= (enemy-size enemy) ENEMY-LARGE) LARGE-POINTS])) score))
|#

;;---------Doomstick:----------

;; player-or-all-enemies-eaten?: FishWorld -> Boolean
;; Consumes:
;;  - FishWorld fw: the current state of the game
;; Produces: whether or not either the Player or all of the enemies
;;           in fw are gone
(check-expect (player-or-all-enemies-eaten?
               (make-fish-world 'eaten 
                                LOE1
                                0))
              #true)
(check-expect (player-or-all-enemies-eaten?
               (make-fish-world PLAYER1
                                '()
                                10))
              #true)
(check-expect (player-or-all-enemies-eaten? START)
              #false)

(define (player-or-all-enemies-eaten? fw)
  (or (eq? (fish-world-player fw) 'eaten) ;Need to update all player-fns if we choose this route...
      (empty? (fish-world-enemies fw))))

(define LOST-TEXT "Game Over")
(define WON-TEXT "You win!")
;; render-doomstick: FishWorld -> Image
;;  Consumes:
;;   - FishWorld fw: the final state of the simulation
;;  Produces: An Image containing the Player or all remaining
;;   Enemies at appropriate locations, the cuurent score in
;;   the upper right corner of the screen, and either the
;;   words "You Win!" or "You Lose!" in the center of the screen
(check-expect (render-doomstick
               (make-fish-world 'eaten 
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
    (cond [(eq? (fish-world-player fw) 'eaten)
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