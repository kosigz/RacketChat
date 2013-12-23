;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname RacketChat) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
; Racket Chat
; @author Konstantin Gizdarski
; @location Somewhere over North America on a flight from BOS -> SFO
; @date Saturday, 14 December 2013
; ------------------------------------------------------------------------------
(require 2htdp/image)
(require 2htdp/universe)
; ------------------------------------------------------------------------------
; This is a multi-person chat program which allows users to send messages to all
; the participants of the conversation. Messages are limited to one line.
; ------------------------------------------------------------------------------
; DATA DEFINITIONS
; ------------------------------------------------------------------------------
; A ClientState is:
; -- (list String[TextBox] [List-of Message])
; A Message is:
; -- (list String[User] String[Text])
; A Client2Server is:
; -- String
; A Server2Client is:
; -- Message
; ------------------------------------------------------------------------------
; CONSTANTS
; ------------------------------------------------------------------------------
(define WIDTH 600)
(define HEIGHT 400)
(define SEND-BUTTON (overlay (text "SEND" 14 'black) 
                             (rectangle 50 30 'solid 'green)))
(define TEXT-BOX (rectangle (- WIDTH 70) 25 'outline 'black))
(define BACKGROUND (place-image SEND-BUTTON (- WIDTH 30) (- HEIGHT 20)
                                (empty-scene WIDTH HEIGHT)))
; ------------------------------------------------------------------------------
; SAMPLE MESSAGES (SERVER2CLIENT)
; ------------------------------------------------------------------------------
(define M1 (list "kosig" "Hey"))
(define M2 (list "joe" "Hey! What's up man?"))
(define M3 (list "thay" "Not much dude. I'm just here."))
(define M4 (list "mkol" "Did you ever finish that problem set?"))
(define M5 (list "1234" "6789012345678901"))
(define CLIENT (list "Some TEXT" (list M1 M2 M3 M3 M1 M2 M3 M3 M1 M2 M3 M3 M1)))
; ------------------------------------------------------------------------------
; CLIENT MAIN FUNCTION
; ------------------------------------------------------------------------------
; join-convo: String[Username] String[IP] -> ClientState
; interp. join the chat room in the given IP address with the given username
(define (join username ip)
  (big-bang (list "" empty)
            (to-draw render)
            (on-receive process)
            (on-key apply-stroke)
            (on-mouse send)
            (name username)
            (register ip)))
; ------------------------------------------------------------------------------
; CLIENT HANDLER FUNCTIONS
; ------------------------------------------------------------------------------
; render: ClientState -> Image
; interp. draws a client state to the screen
(define (render client)
  (local [(define textbox (first client))
          (define textbox-img (text textbox 13 'black))
          (define width/t (image-width textbox-img))
          (define archive (map message->text (second client)))
          (define (render/h archive offset)
            (cond
              [(empty? archive) BACKGROUND]
              [else (local [(define current (first archive))
                            (define as-img (text current 13 'black))
                            (define width (image-width as-img))]
                      (place-image as-img (+ (/ width 2) 10) offset
                                   (render/h (rest archive) (+ 15 offset))))]))]
    (place-image textbox-img (+ (/ width/t 2) 10) (- HEIGHT 15)
                 (render/h archive 20))))

; process: ClientState Server2Client -> ClientState
; interp. given a new message, adds it to the list of messages
(define (process client server2client)
  (local [(define messages (second client))]
    (cond
      [(>= (length messages) 22)
       (list (first client) (append (rest messages) (list server2client)))]
      [else
       (list (first client) (append messages (list server2client)))])))

; apply-stroke: ClientState KeyEvent -> ClientState
; interp. adds the current key stroke to the line that is being typed.
; Each line is limited to 100 characters.
(define (apply-stroke client key)
  (cond
    [(< 90 (string-length (first client)))
     client]
    [else 
     (cond
       [(string=? "\r" key)
        (send client WIDTH HEIGHT "button-down")]
       [(string=? "\b" key)
        (list (backspace (first client)) (second client))]
       [(= (string-length key) 1)
        (list (string-append (first client) key) (second client))]
       [else
        client])]))

; on-mouse: ClientState Number[X] Number[Y] MouseEvent -> HandlerResult
; interp. sends the current message if the user clicks around the send box
; used to supplement the same functionality using the enter key
(define (send client x y event)
  (if (and (> x (- WIDTH 60)) (> y (- HEIGHT 40)) (string=? event "button-down"))
      (make-package (list "" (second client)) (first client))
      client))
; ------------------------------------------------------------------------------
; CLIENT UTILITY FUNCTIONS
; ------------------------------------------------------------------------------
; message->text: Message -> String
; interp. converts an incoming message into text
(define (message->text message)
  (string-append (first message) ": " (second message)))

; backspace: String -> String
; interp. deletes the last character in the given string
(define (backspace string)
  (cond
    [(<= (string-length string) 1) ""]
    [else
     (string-append (substring string 0 1)
                    (backspace (substring string 1)))]))

; ------------------------------------------------------------------------------
; SERVER DATA DEFINITIONS
; ------------------------------------------------------------------------------
; A ServerState is:
; -- (list Number[Capacity] [List-of IWorld])
; A Message is:
; -- (list String[User] String[Text])
; A Client2Server is:
; -- String
; A Server2Client is:
; -- Message
; ------------------------------------------------------------------------------
; SERVER MAIN FUNCTION
; ------------------------------------------------------------------------------
; new-room: Number[Capacity] -> ...
; interp. creates a new chat room with a capacity of n users
(define (new-room n)
  (universe (list n empty)
            (on-msg update)
            (on-new add-client)))
; ------------------------------------------------------------------------------
; SERVER HANDLER FUNCTIONS
; ------------------------------------------------------------------------------
; update: ServerState IWorld Client2Server -> UniverseBundle
; interp. sends the message to every single world
(define (update server world client2server)
  (local [(define message (list (iworld-name world) client2server))
          (define (inner-net recipient)
            (make-mail recipient message))]
    (make-bundle server
                 (map inner-net (second server))
                 empty)))

; add-client: ServerState IWorld -> UniverseBundle
; interp. adds the given client to the world if the chatroom is not filled, else
; ignores the request
(define (add-client server world)
  (local [(define primero (first server))
          (define segundo (second server))]
    (cond
      [(= primero (length segundo))
       (make-bundle server empty empty)]
      [else
       (local [(define new (append segundo (list world)))]
         (make-bundle (list primero new)
                      empty
                      empty))])))