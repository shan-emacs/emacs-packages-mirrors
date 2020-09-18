;;; sv-kalender-namnsdagar.el --- Swedish celebrated name of the day -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Mats Lidell

;; Author: Mats Lidell <mats.lidell@lidells.se>
;; Version: 0.9
;; Package-Version: 20190421.1521
;; Package-Commit: fff970f49c77abfc69e37817f25a939818420971
;; Keywords: calendar, swedish, localization
;; URL: https://github.com/matsl/sv-kalender-namnsdagar

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Swedish celebrated name of the day

;; Example usage
;;
;; Add to your .diary file
;; &%%(sv-kalender-namnsdagar)

;;; Code:

(require 'calendar)

(defconst sv-kalender-namnsdagar-name-list
  '(
    (                                   ; Januari
     ("Nyårsdagen")                     ; 1
     ("Svea")                           ; 2
     ("Alfred" "Alfrida")               ; 3
     ("Rut")                            ; 4
     ("Hanna" "Hannele")                ; 5
     ("Kasper" "Melker" "Baltsar")      ; 6
     ("August" "Augusta")               ; 7
     ("Erland")                         ; 8
     ("Gunnar" "Gunder")                ; 9
     ("Sigurd" "Sigbritt")              ;10
     ("Jan" "Jannike")                  ;11
     ("Frideborg" "Fridolf")            ;12
     ("Knut")                           ;13
     ("Felix" "Felicia")                ;14
     ("Laura" "Lorentz")                ;15
     ("Hjalmar" "Helmer")               ;16
     ("Anton" "Tony")                   ;17
     ("Hilda" "Hildur")                 ;18
     ("Henrik")                         ;19
     ("Fabian" "Sebastian")             ;20
     ("Agnes" "Agneta")                 ;21
     ("Vincent" "Viktor")               ;22
     ("Frej" "Freja")                   ;23
     ("Erika")                          ;24
     ("Paul" "Pål")                     ;25
     ("Bodil" "Boel")                   ;26
     ("Göte" "Göta")                    ;27
     ("Karl" "Karla")                   ;28
     ("Diana")                          ;29
     ("Gunilla" "Gunhild")              ;30
     ("Ivar" "Joar")                    ;31
     )
    (                                   ; Februari
     ("Max" "Maximilian")               ; 1
     ("Kyndelsmässodagen")              ; 2
     ("Disa" "Hjördis")                 ; 3
     ("Ansgar" "Anselm")                ; 4
     ("Agata" "Agda")                   ; 5
     ("Dorotea" "Doris")                ; 6
     ("Rikard" "Dick")                  ; 7
     ("Berta" "Bert")                   ; 8
     ("Fanny" "Franciska")              ; 9
     ("Iris")                           ;10
     ("Yngve" "Inge")                   ;11
     ("Evelina" "Evy")                  ;12
     ("Agne" "Ove")                     ;13
     ("Valentin")                       ;14
     ("Sigfrid")                        ;15
     ("Julia" "Julius")                 ;16
     ("Alexandra" "Sandra")             ;17
     ("Frida" "Fritiof")                ;18
     ("Gabriella" "Ella")               ;19
     ("Vivianne")                       ;20
     ("Hilding")                        ;21
     ("Pia")                            ;22
     ("Torsten" "Torun")                ;23
     ("Mattias" "Mats")                 ;24
     ("Sigvard" "Sivert")               ;25
     ("Torgny" "Torkel")                ;26
     ("Lage")                           ;27
     ("Maria")                          ;28
     ("Skottdagen")                     ;29
     )
    (                                   ; Mars
     ("Albin" "Elvira")                 ; 1
     ("Ernst" "Erna")                   ; 2
     ("Gunborg" "Gunvor")               ; 3
     ("Adrian" "Adriana")               ; 4
     ("Tora" "Tove")                    ; 5
     ("Ebba" "Ebbe")                    ; 6
     ("Camilla")                        ; 7
     ("Siv" "Saga")                     ; 8
     ("Torbjörn" "Torleif")             ; 9
     ("Edla" "Ada")                     ;10
     ("Edvin" "Egon")                   ;11
     ("Viktoria")                       ;12
     ("Greger")                         ;13
     ("Matilda" "Maud")                 ;14
     ("Kristoffer" "Christel")          ;15
     ("Herbert" "Gilbert")              ;16
     ("Gertrud")                        ;17
     ("Edvard" "Edmund")                ;18
     ("Josef" "Josefina")               ;19
     ("Joakim" "Kim")                   ;20
     ("Bengt")                          ;21
     ("Kennet" "Kent")                  ;22
     ("Gerda" "Gerd")                   ;23
     ("Gabriel" "Rafael")               ;24
     ("Marie bebådelsedag")             ;25
     ("Emanuel")                        ;26
     ("Rudolf" "Ralf")                  ;27
     ("Malkolm" "Morgan")               ;28
     ("Jonas" "Jens")                   ;29
     ("Holger" "Holmfrid")              ;30
     ("Ester")                          ;31
     )
    (                                   ; April
     ("Harald" "Hervor")                ; 1
     ("Gudmund" "Ingemund")             ; 2
     ("Ferdinand" "Nanna")              ; 3
     ("Marianne" "Marlene")             ; 4
     ("Irene" "Irja")                   ; 5
     ("Vilhelm" "William")              ; 6
     ("Irma" "Irmelin")                 ; 7
     ("Nadja" "Tanja")                  ; 8
     ("Otto" "Ottilia")                 ; 9
     ("Ingvar" "Ingvor")                ;10
     ("Ulf" "Ylva")                     ;11
     ("Liv")                            ;12
     ("Artur" "Douglas")                ;13
     ("Tiburtius")                      ;14
     ("Olivia" "Oliver")                ;15
     ("Patrik" "Patricia")              ;16
     ("Elias" "Elis")                   ;17
     ("Valdemar" "Volmar")              ;18
     ("Olaus" "Ola")                    ;19
     ("Amalia" "Amelie")                ;20
     ("Anneli" "Annika")                ;21
     ("Allan" "Glenn")                  ;22
     ("Georg" "Göran")                  ;23
     ("Vega")                           ;24
     ("Markus")                         ;25
     ("Teresia" "Terese")               ;26
     ("Engelbrekt")                     ;27
     ("Ture" "Tyra")                    ;28
     ("Tyko")                           ;29
     ("Mariana")                        ;30
     )
    (                                   ; Maj
     ("Valborg")                        ; 1
     ("Filip" "Filippa")                ; 2
     ("John" "Jane")                    ; 3
     ("Monika" "Mona")                  ; 4
     ("Gotthard" "Erhard")              ; 5
     ("Marit" "Rita")                   ; 6
     ("Carina" "Carita")                ; 7
     ("Åke")                            ; 8
     ("Reidar" "Reidun")                ; 9
     ("Esbjörn" "Styrbjörn")            ;10
     ("Märta" "Märit")                  ;11
     ("Charlotta" "Lotta")              ;12
     ("Linnea" "Linn")                  ;13
     ("Halvard" "Halvar")               ;14
     ("Sofia" "Sonja")                  ;15
     ("Ronald" "Ronny")                 ;16
     ("Rebecka" "Ruben")                ;17
     ("Erik")                           ;18
     ("Maj" "Majken")                   ;19
     ("Karolina" "Carola")              ;20
     ("Konstantin" "Conny")             ;21
     ("Hemming" "Henning")              ;22
     ("Desideria" "Desirée")            ;23
     ("Ivan" "Vanja")                   ;24
     ("Urban")                          ;25
     ("Vilhelmina" "Vilma")             ;26
     ("Beda" "Blenda")                  ;27
     ("Ingeborg" "Borghild")            ;28
     ("Yvonne" "Jeanette")              ;29
     ("Vera" "Veronika")                ;30
     ("Petronella" "Pernilla")          ;31
     )
    (                                   ; Juni
     ("Gun" "Gunnel")                   ; 1
     ("Rutger" "Roger")                 ; 2
     ("Ingemar" "Gudmar")               ; 3
     ("Solbritt" "Solveig")             ; 4
     ("Bo")                             ; 5
     ("Gustav" "Gösta")                 ; 6
     ("Robert" "Robin")                 ; 7
     ("Eivor" "Majvor")                 ; 8
     ("Börje" "Birger")                 ; 9
     ("Svante" "Boris")                 ;10
     ("Bertil" "Berthold")              ;11
     ("Eskil")                          ;12
     ("Aina" "Aino")                    ;13
     ("Håkan" "Hakon")                  ;14
     ("Margit" "Margot")                ;15
     ("Axel" "Axelina")                 ;16
     ("Torborg" "Torvald")              ;17
     ("Björn" "Bjarne")                 ;18
     ("Germund" "Görel")                ;19
     ("Linda")                          ;20
     ("Alf" "Alvar")                    ;21
     ("Paulina" "Paula")                ;22
     ("Adolf" "Alice")                  ;23
     ("Johannes Döparens dag")          ;24
     ("David" "Salomon")                ;25
     ("Rakel" "Lea")                    ;26
     ("Selma" "Fingal")                 ;27
     ("Leo")                            ;28
     ("Peter" "Petra")                  ;29
     ("Elof" "Leif")                    ;30
     )
    (                                   ; Juli
     ("Aron" "Mirjam")                  ; 1
     ("Rosa" "Rosita")                  ; 2
     ("Aurora")                         ; 3
     ("Ulrika" "Ulla")                  ; 4
     ("Laila" "Ritva")                  ; 5
     ("Esaias" "Jessika")               ; 6
     ("Klas")                           ; 7
     ("Kjell")                          ; 8
     ("Jörgen" "Örjan")                 ; 9
     ("André" "Andrea")                 ;10
     ("Eleonora" "Ellinor")             ;11
     ("Herman" "Hermine")               ;12
     ("Joel" "Judit")                   ;13
     ("Folke")                          ;14
     ("Ragnhild" "Ragnvald")            ;15
     ("Reinhold" "Reine")               ;16
     ("Bruno")                          ;17
     ("Fredrik" "Fritz")                ;18
     ("Sara")                           ;19
     ("Margareta" "Greta")              ;20
     ("Johanna")                        ;21
     ("Magdalena" "Madeleine")          ;22
     ("Emma" "Emmy")                    ;23
     ("Kristina" "Kerstin")             ;24
     ("Jakob")                          ;25
     ("Jesper" "Jasmin")		;26
     ("Marta")                          ;27
     ("Botvid" "Seved")                 ;28
     ("Olof")                           ;29
     ("Algot")                          ;30
     ("Helena" "Elin")                  ;31
     )
    (                                   ; Augusti
     ("Per")                            ; 1
     ("Karin" "Kajsa")                  ; 2
     ("Tage")                           ; 3
     ("Arne" "Arnold")                  ; 4
     ("Ulrik" "Alrik")                  ; 5
     ("Alfons" "Inez")                  ; 6
     ("Dennis" "Denise")                ; 7
     ("Silvia" "Sylvia")                ; 8
     ("Roland")                         ; 9
     ("Lars")                           ;10
     ("Susanna")                        ;11
     ("Klara")                          ;12
     ("Kaj")                            ;13
     ("Uno")                            ;14
     ("Stella" "Estelle")               ;15
     ("Brynolf")                        ;16
     ("Verner" "Valter")                ;17
     ("Ellen" "Lena")                   ;18
     ("Magnus" "Måns")                  ;19
     ("Bernhard" "Bernt")               ;20
     ("Jon" "Jonna")                    ;21
     ("Henrietta" "Henrika")            ;22
     ("Signe" "Signhild")               ;23
     ("Bartolomeus")                    ;24
     ("Lovisa" "Louise")                ;25
     ("Östen")                          ;26
     ("Rolf" "Raoul")                   ;27
     ("Fatima" "Leila")			;28
     ("Hans" "Hampus")                  ;29
     ("Albert" "Albertina")             ;30
     ("Arvid" "Vidar")                  ;31
     )
    (                                   ; September
     ("Sam" "Samuel")                   ; 1
     ("Justus" "Justina")               ; 2
     ("Alfhild" "Alva")                 ; 3
     ("Gisela")                         ; 4
     ("Adela" "Heidi")                  ; 5
     ("Lilian" "Lilly")                 ; 6
     ("Kevin" "Roy")                    ; 7
     ("Alma" "Hulda")                   ; 8
     ("Anita" "Annette")                ; 9
     ("Tord" "Turid")                   ;10
     ("Dagny" "Helny")                  ;11
     ("Åsa" "Åslög")                    ;12
     ("Sture")                          ;13
     ("Ida" "Ronja")                    ;14
     ("Sigrid" "Siri")                  ;15
     ("Dag" "Daga")                     ;16
     ("Hildegard" "Magnhild")           ;17
     ("Orvar")                          ;18
     ("Fredrika")                       ;19
     ("Elise" "Lisa")                   ;20
     ("Matteus")                        ;21
     ("Maurits" "Moritz")               ;22
     ("Tekla" "Tea")                    ;23
     ("Gerhard" "Gert")                 ;24
     ("Tryggve")                        ;25
     ("Enar" "Einar")                   ;26
     ("Dagmar" "Rigmor")                ;27
     ("Lennart" "Leonard")              ;28
     ("Mikael" "Mikaela")               ;29
     ("Helge")                          ;30
     )
    (                                   ; Oktober
     ("Ragnar" "Ragna")                 ; 1
     ("Ludvig" "Love")                  ; 2
     ("Evald" "Osvald")                 ; 3
     ("Frans" "Frank")                  ; 4
     ("Bror")                           ; 5
     ("Jenny" "Jennifer")               ; 6
     ("Birgitta" "Britta")              ; 7
     ("Nils")                           ; 8
     ("Ingrid" "Inger")                 ; 9
     ("Harry" "Harriet")                ;10
     ("Erling" "Jarl")                  ;11
     ("Valfrid" "Manfred")              ;12
     ("Berit" "Birgit")                 ;13
     ("Stellan")                        ;14
     ("Hedvig" "Hillevi")               ;15
     ("Finn")                           ;16
     ("Antonia" "Toini")                ;17
     ("Lukas")                          ;18
     ("Tore" "Tor")                     ;19
     ("Sibylla")                        ;20
     ("Ursula" "Yrsa")                  ;21
     ("Marika" "Marita")                ;22
     ("Severin" "Sören")                ;23
     ("Evert" "Eilert")                 ;24
     ("Inga" "Ingalill")                ;25
     ("Amanda" "Rasmus")                ;26
     ("Sabina")                         ;27
     ("Simon" "Simone")                 ;28
     ("Viola")                          ;29
     ("Elsa" "Isabella")                ;30
     ("Edit" "Edgar")                   ;31
     )
    (                                   ; November
     ("Allhelgonadagen")                ; 1
     ("Tobias")                         ; 2
     ("Hubert" "Hugo")                  ; 3
     ("Sverker")                        ; 4
     ("Eugen" "Eugenia")                ; 5
     ("Gustav" "Adolf")                 ; 6
     ("Ingegerd" "Ingela")              ; 7
     ("Vendela")                        ; 8
     ("Teodor" "Teodora")               ; 9
     ("Martin" "Martina")               ;10
     ("Mårten")                         ;11
     ("Konrad" "Kurt")                  ;12
     ("Kristian" "Krister")             ;13
     ("Emil" "Emilia")                  ;14
     ("Leopold")                        ;15
     ("Vibeke" "Viveka")                ;16
     ("Naemi" "Naima")                  ;17
     ("Lillemor" "Moa")                 ;18
     ("Elisabet" "Lisbet")              ;19
     ("Pontus" "Marina")                ;20
     ("Helga" "Olga")                   ;21
     ("Cecilia" "Sissela")              ;22
     ("Klemens")                        ;23
     ("Gudrun" "Rune")                  ;24
     ("Katarina" "Katja")               ;25
     ("Linus")                          ;26
     ("Astrid" "Asta")                  ;27
     ("Malte")                          ;28
     ("Sune")                           ;29
     ("Andreas" "Anders")               ;30
     )
    (                                   ; December
     ("Oskar" "Ossian")                 ; 1
     ("Beata" "Beatrice")               ; 2
     ("Lydia")                          ; 3
     ("Barbara" "Barbro")               ; 4
     ("Sven")                           ; 5
     ("Nikolaus" "Niklas")              ; 6
     ("Angela" "Angelika")              ; 7
     ("Virginia")                       ; 8
     ("Anna")                           ; 9
     ("Malin" "Malena")                 ;10
     ("Daniel" "Daniela")               ;11
     ("Alexander" "Alexis")             ;12
     ("Lucia")                          ;13
     ("Sten" "Sixten")                  ;14
     ("Gottfrid")                       ;15
     ("Assar")                          ;16
     ("Stig")                           ;17
     ("Abraham")                        ;18
     ("Isak")                           ;19
     ("Israel" "Moses")                 ;20
     ("Tomas")                          ;21
     ("Natanael" "Jonatan")             ;22
     ("Adam")                           ;23
     ("Eva")                            ;24
     ("Juldagen")                       ;25
     ("Stefan" "Staffan")               ;26
     ("Johannes" "Johan")               ;27
     ("Benjamin" "Värnlösa barns dag")  ;28
     ("Natalia" "Natalie")              ;29
     ("Abel" "Set")                     ;30
     ("Sylvester")                      ;31
     ))
  "Swedish namesdays according to the 2001 revision."
  )

;; To be called from diary-list-sexp-entries, where DATE is bound.
(defun sv-kalender-namnsdagar ()
  "The days celebrated name in Sweden."
  (let* ((day (calendar-extract-day date))
	 (month (calendar-extract-month date))
	 (names (nth (- day 1) (nth (- month 1) sv-kalender-namnsdagar-name-list))))
    (cond ((null names)
	   (format "Inga namn idag, OK??"))
	  ((= 3 (length names))
	   (format "Dagens namn är: %s, %s och %s" (car names) (nth 1 names) (nth 2 names)))
	  ((= 2 (length names))
	   (format "Dagens namn är: %s och %s" (car names) (nth 1 names)))
	  (t (format "Dagens namn är: %s" (car names))))))

(provide 'sv-kalender-namnsdagar)
;;; sv-kalender-namnsdagar.el ends here
