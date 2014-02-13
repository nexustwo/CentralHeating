// 2001

$crystal = 8000000
$regfile = "8535def.dat"


'inicjacja LCD
Config Lcd = 16 * 2
Config Lcdpin = Pin , Db4 = Portb.4 , Db5 = Portb.3 , Db6 = Portb.2 , Db7 = Portb.1 , E = Portb.5 , Rs = Portb.6
Cursor Off
Cls

'up
Deflcdchar 0 , 32 , 4 , 14 , 31 , 4 , 4 , 32 , 32             ' replace ? with number (0-7)

'wewn
Deflcdchar 1 , 4 , 14 , 31 , 31 , 21 , 23 , 31 , 32
'piec
Deflcdchar 2 , 31 , 17 , 25 , 31 , 17 , 25 , 31 , 32
'pompa
Deflcdchar 3 , 32 , 19 , 30 , 14 , 15 , 25 , 32 , 32
'wentylator
Deflcdchar 4 , 32 , 25 , 11 , 4 , 26 , 19 , 32 , 32
'stopień
Deflcdchar 5 , 6 , 9 , 9 , 6 , 32 , 32 , 32 , 32

'down
Deflcdchar 6 , 32 , 4 , 4 , 31 , 14 , 4 , 32 , 32             ' replace ? with number (0-7)

'drzwi
Deflcdchar 7 , 32 , 31 , 17 , 17 , 25 , 17 , 31 , 32

'inicjacja portów
Set Ddrb.0
Led Alias Portb.0
Reset Led

Set Portc.7
Set Portc.6
Set Portc.5
Set Portc.4
Sw_menu Alias Pinc.7
Sw_up Alias Pinc.6
Sw_run Alias Pinc.5
Sw_down Alias Pinc.4

Ddrd.6 = 1
Pomp Alias Portd.6
Pomp = 1

Ddrc.0 = 0
Portc.0 = 1
Drzwi Alias Pinc.0

Ddrc.1 = 1
Buzz Alias Portc.1
Buzz = 0


'konfiguracja Adc
Config Adc = Single , Prescaler = Auto
Start Adc

'zmienne

'bitowe
Dim Actual As Bit
Dim Run As Bit
Dim Run_f As Bit
Dim Pres As Bit
Dim Wreep As Bit
Dim Once As Bit
Dim Pompa As Bit
Dim Buzz_once As Bit
Dim Act_pid As Bit

'wyświetlanie
Dim Obroty_f As Bit
Dim Tdom_err As Bit
Dim Tpiec_err As Bit
Dim Tzew_err As Bit
Dim Show_f As Bit

'odsługa parametrów procesu
Dim P_up(7) As Byte
Dim P_down(7) As Byte
Dim Menu As Byte
Dim P(7) As Byte

'pomiar tempetatur
Dim T_piec As Byte
Dim T_dom As Byte
Dim T_domd As Byte
Dim T_zew As Integer

Dim Mem_dom As Word
Dim Mem_piec As Word
Dim Mem_zew As Word

'zmienne licznikowe
Dim F As Byte

Dim Timeoff As Word
Dim Pid As Byte
Dim Powrot As Byte
Dim Repeat As Byte
Dim Licz As Byte
Dim Podsw As Byte
Dim Pid_inc As Byte

'zmienne pomocnicze
Dim X As Word
Dim Temp As Word
Dim Temp1 As Word
Dim Eep As Byte

'zmienne pomocnicze PID
Dim T_piec_temp As Byte
Dim A As Integer
Dim Integ As Integer
Dim Diver As Byte
Dim Diff As Integer

'parametry PID
Dim Kp As Word
Dim Ki As Word
Dim Kd As Word
Dim Odch As Integer

'zmienne wyjściowe
Dim Went As Long
Dim Went1 As Integer

Dim Chan As Byte
Declare Sub Odczyt_t(byval Chan As Byte)

'ustalenie granic zakresów
P_up(1) = 0
P_up(2) = 85
P_up(3) = 50
P_up(4) = 30
P_up(5) = 1
P_up(6) = 100
P_up(7) = 50

P_down(1) = 0
P_down(2) = 50
P_down(3) = 30
P_down(4) = 10
P_down(5) = 0
P_down(6) = 50
P_down(7) = 0

'odczytanie parametrów z eeprom
Readeeprom Eep , 1
If Eep = 165 Then
   For F = 2 To 7
      Readeeprom Eep , F
      P(f) = Eep
   Next F
Else
'wartości domyślne
   P(2) = 70
   P(3) = 50
   P(4) = 21
   P(5) = 0
   P(6) = 100
   P(7) = 10
   Temp = 165
   Writeeeprom Eep , 1
   For F = 2 To 7
      Eep = P(f)
      Writeeeprom Eep , F
   Next F
End If

'początkowe parametry
'Went1 = 0
Menu = 1
Run = 0
Run_f = 0
'Pompa = 1
'Repeat = 0
Licz = 0
Actual = 1
'Pid = 0

Kp = 166

'podzielić przez 10
Ki = 1

'podzielić przez 6
Kd = 1000

Once = 0
Podsw = 255
Powrot = 0

Cls

Lcd "  Sterownik do  "
Lowerline
Lcd "pieca CO ver 3.1"
Wait 4
Podsw = 255
Upperline
Lcd "Programming by: "
Lowerline
Lcd " Pawel B**"
Wait 4

Podsw = 255

Gosub Tik
Waitms 100
Gosub Tik


Cls

Config Timer1 = Timer , Prescale = 8
Start Timer1
Enable Interrupts
Enable Timer1
On Timer1 Tim1

Start Timer2
Config Timer2 = Pwm , Async = Off , Pwm = On , Compare = Set , Prescale = 256 . Compare Pwm = Clear Up
Tccr2 = &B01100100
Ocr2 = 0

Do
'obsługa przycisków
   If Pres = 0 Then
      If Sw_menu = 0 Then
         Pres = 1
         Waitms 50
         If Sw_menu = 0 Then
            Gosub Tik
            Podsw = 255
            Powrot = 100
            Incr Menu
            If Menu = 8 Then Menu = 2
            Gosub Refresh
         End If
      Elseif Sw_run = 0 Then
         Pres = 1
         Waitms 50
         If Sw_run = 0 Then
            Gosub Tik
            Podsw = 255
            Cls
            Licz = 0
            Actual = 1
            If Menu = 1 Then
               Toggle Run_f
               If Run_f = 1 Then
                  Run = 1
                  If P(5) = 0 Then
                     P(1) = P(2)
                  Else
                     If T_dom >= P(4) Then
                        P(1) = 50
                     Else
                        P(1) = P(2)
                     End If
                  End If
                  Act_pid = 1
                  Integ = 0
                  Diver = 0
                  Timeoff = 3600
                  T_piec_temp = T_piec
                  Show_f = 1
               Else
                  Run = 0
               End If
            Else
               Menu = 1
               Wreep = 1
               Actual = 1
            End If
         End If
      Elseif Sw_up = 0 Then
         Pres = 1
         Waitms 50
         If Sw_up = 0 Then
            Gosub Tik
            Powrot = 100
            Podsw = 255
            If Menu > 1 Then
               If P(menu) < P_up(menu) Then Incr P(menu)
               Gosub Par
            End If
         End If
      Elseif Sw_down = 0 Then
         Pres = 1
         Waitms 50
         If Sw_down = 0 Then
            Gosub Tik
            Powrot = 100
            Podsw = 255
            If Menu > 1 Then
               If P(menu) > P_down(menu) Then Decr P(menu)
               Gosub Par
            End If
         End If
      End If
   End If


   If Sw_menu = 1 And Sw_run = 1 And Sw_up = 1 And Sw_down = 1 Then
      Pres = 0
      Repeat = 0
   End If


If Drzwi = 0 And Once = 0 Then
   Ocr2 = Went
   Once = 1
End If

If Drzwi = 1 Then
   Ocr2 = 0
   Once = 0
End If

If Wreep = 1 Then
   Wreep = 0
   Eep = 165
   Writeeeprom Eep , 1
   For F = 2 To 7
      Eep = P(f)
      Writeeeprom Eep , F
   Next F
End If


If Actual = 1 Then
   Actual = 0


'Odczyt Temperatur

'dom

   Call Odczyt_t(0)

   If Temp > Mem_dom Then Decr Temp
   If Temp < Mem_dom Then Incr Temp
   Mem_dom = Temp
   Temp1 = Temp
   Temp = Temp / 5
   T_dom = Temp
   T_domd = Temp1
   T_domd = T_domd Mod 5
   T_domd = T_domd * 2

'piec

   Call Odczyt_t(1)

   If Temp > Mem_piec Then Decr Temp
   If Temp < Mem_piec Then Incr Temp
   Mem_piec = Temp
   Temp = Temp / 5
   T_piec = Temp

'zewnętrzna

   Call Odczyt_t(2)

   If Temp > Mem_zew Then Decr Temp
   If Temp < Mem_zew Then Incr Temp
   Mem_zew = Temp
   T_zew = Temp
   T_zew = T_zew - 512
   T_zew = T_zew / 5


'sprawdzenie poprawności czujników
   If T_piec > 1 And T_piec < 110 Then
      Tpiec_err = 0
   Else
      Tpiec_err = 1
   End If

   If T_dom > 1 And T_dom < 50 Then
      Tdom_err = 0
   Else
      Tdom_err = 1
   End If

   If T_zew > -50 And T_zew < 50 Then
      Tzew_err = 0
   Else
      Tzew_err = 1
   End If

   If Tpiec_err = 1 Then
      Run = 0
      Went1 = 0
      Ocr2 = 0
   End If

   If Tdom_err = 1 And P(5) = 1 Then
      Run = 0
      Went1 = 0
      Ocr2 = 0
   End If

   If Run = 0 And Run_f = 1 Then
      Gosub Tik
   End If

   If T_piec >= P(3) Then Timeoff = 0

   If Tpiec_err = 0 Then
      Temp = P(3) + 5
      If T_piec >= Temp Then
         Pompa = 0
         Pomp = 0
      Elseif T_piec < P(3) Then
         Pompa = 1
         Pomp = 1
      End If
   Else
      Pompa = 1
      Pomp = 1
   End If

   If Timeoff > 0 Then
      Decr Timeoff
      If Timeoff = 0 Then
         Run = 0
         Run_f = 0
         Cls
      End If
   End If

      If Menu = 1 Then
'wyświetlanie menu główne
      Toggle Show_f
      Gosub Wysw
   End If

End If


'Obliczenie Obrotów Wentylatora
'Regulator Pid
If Run = 1 Then

   If Act_pid = 1 Then
      Act_pid = 0

'wywołanie PID do ok 10s

         If P(5) = 0 Then
            P(1) = P(2)
         Else
            If T_dom >= P(4) Then
               P(1) = 50
            Else
               P(1) = P(2)
            End If
         End If

'odchyłka
         Odch = P(1)
         Odch = Odch - T_piec

'prop
         A = Odch
         A = A * Kp
         Went1 = A

'Wspołczynnik Całkujący co ok 1min
         A = Odch
         A = A * Ki
         Integ = Integ + A
         If Integ > 2000 Then Integ = 2000
         If Integ < 0 Then Integ = 0
         Went1 = Went1 + Integ

'wsp różniczkujący
         Incr Diver

         If Diver = 6 Then
            Diver = 0
            A = T_piec_temp
            A = A - T_piec

            A = A * Kd
            Diff = A
         End If

         Went1 = Went1 + Diff

'aktualizacja obr went
         Temp = P(6)
         Temp = Temp * 100
         If Went1 > Temp Then Went1 = Temp

         If Went1 < 0 Then Went1 = 0

'sprawdzanie zakresów
         Went = Went1
         Temp = P(7)
         Temp = Temp * 100
         If Went < Temp Then Went = 0
         Went = Went * 255
         Went = Went / 10000

         If Drzwi = 0 Then
            Ocr2 = Went
         Else
            Ocr2 = 0
         End If

         T_piec_temp = T_piec


      Temp = Went1
      Temp = Temp / 100
      If Timeoff = 0 And T_piec < P(3) And Temp = P(6) Then
         Run = 0
         Run_f = 0
         Cls
      End If
   End If
Else

   Went = 0
   Ocr2 = 0
   Pid = 0
End If





Loop

'procedury


'odczyt temp
Sub Odczyt_t(chan)
   Temp = 0
   For F = 1 To 16
      X = Getadc(chan)
      Temp = Temp + X
   Next F
   Temp = Temp / 16
End Sub



'wyświetlanie tempperatur i itp
Wysw:
   Locate 1 , 1

   If Run_f = 1 Then
      If P(5) = 0 Then
         If Tdom_err = 0 Then
            Lcd Chr(1) ; T_dom ; "." ; T_domd ; Chr(5) ; "C "
         Else
            Lcd "      "
         End If

         Locate 1 , 9
         If Tpiec_err = 0 Then
            Lcd Chr(2) ; T_piec ; "/" ; P(2) ; Chr(5) ; "C "
         Else
            If Show_f = 1 Then
               Lcd Chr(2) ; "???    "
            Else
               Lcd "       "
            End If
         End If

      Elseif P(5) = 1 Then
         If Tdom_err = 0 Then
            Lcd Chr(1) ; T_dom ; "." ; T_domd
            Lcd "/" ; P(4) ; Chr(5) ; "C "
         Else
            If Show_f = 1 Then
               Lcd Chr(1) ; "???       "
            Else
               Lcd "          "
            End If
         End If

         Locate 1 , 12
         If Tpiec_err = 0 Then
            Lcd Chr(2) ; T_piec ; Chr(5) ; "C "
         Else
            If Show_f = 1 Then
               Lcd Chr(2) ; "??? "
            Else
               Lcd "    "
            End If
         End If
      End If

      Locate 2 , 9

      If Drzwi = 1 Then
         Obroty_f = Show_f
      Else
         Obroty_f = 1
      End If

      If Obroty_f = 1 Then
         Temp = Went1
         Temp = Temp / 100
         Lcd Chr(4) ; Temp ; "% "
      Else
         Lcd "     "
      End If

   Else
      If Tdom_err = 0 Then
         Lcd Chr(1) ; T_dom ; "." ; T_domd ; Chr(5) ; "C "
      Else
         Lcd "       "
      End If

      Locate 1 , 9

      If Tpiec_err = 0 Then
         If T_piec >= P(3) Then
            Lcd Chr(2) ; T_piec ; Chr(5) ; "C "
         Else
            Lcd "     "
         End If
      Else
         Lcd Chr(2) ; "??? "
      End If
   End If

   Locate 2 , 1
   If Tzew_err = 0 Then
      Lcd "*" ; T_zew ; Chr(5) ; "C "
   Else
      Lcd "      "
   End If

   Locate 2 , 15
   If Tpiec_err = 0 And Pompa = 0 Then
      Lcd Chr(3)
   Else
      Lcd " "
   End If

   If Drzwi = 1 Then
      Lcd Chr(7)
   Else
      Lcd " "
   End If

Return


Refresh:
   Cls
   If Menu > 1 Then
      Select Case Menu
         Case 2 : Lcd "1.Max temp.pieca"
         Case 3 : Lcd "2.Min temp.pieca"
         Case 4 : Lcd "3.Temp. w domu  "
         Case 5 : Lcd "4.Sterowanie    "
         Case 6 : Lcd "5.Max obr. went."
         Case 7 : Lcd "6.Min obr. went."
      End Select
      Gosub Par
      Lcd "            "
   End If
Return

Par:
   Locate 2 , 3
   Select Case Menu
      Case 2 : Lcd Chr(2) ; "max=" ; P(2) ; Chr(5) ; "C"
      Case 3 : Lcd Chr(2) ; "off=" ; P(3) ; Chr(5) ; "C"
      Case 4 : Lcd Chr(1) ; "=" ; P(4) ; Chr(5) ; "C"
      Case 5 : If P(5) = 0 Then
                  Lcd Chr(2) ; "max=" ; P(2) ; Chr(5) ; "C"
               Else
                  Lcd Chr(1) ; "=" ; P(4) ; Chr(5) ; "C   "
               End If
      Case 6 : Lcd Chr(4) ; "max=" ; P(6) ; "%  "
      Case 7 : Lcd Chr(4) ; "off=" ; P(7) ; "%  "
   End Select

   Locate 2 , 15
   If P(menu) > P_down(menu) Then
      Lcd Chr(6)
   Else
      Lcd " "
   End If
   If P(menu) < P_up(menu) Then
      Lcd Chr(0)
   Else
      Lcd " "
   End If

Return

Tik:
   Buzz = 1
   Waitms 15
   Buzz = 0
Return


'co 100ms
Tim1:
   If Menu > 1 Then Decr Powrot
   If Powrot = 0 Then
      Menu = 1
      Powrot = 100
      Wreep = 1
      Actual = 1
      Cls
   End If

   If Sw_up = 0 Or Sw_down = 0 Then
      If Repeat < 10 Then Incr Repeat
   End If

   If Repeat >= 10 Then
      If Menu > 1 Then
         If Sw_up = 0 And P(menu) < P_up(menu) Then
            Incr P(menu)
            Gosub Tik
         End If
         If Sw_down = 0 And P(menu) > P_down(menu) Then
            Decr P(menu)
            Gosub Tik
         End If
         Gosub Par
      End If
   End If

'obsługa podswietlania
   If Podsw = 0 Then
      Led = 1
      If Run = 0 And Run_f = 1 Then
         Run_f = 0
         Cls
      End If
   Else
      Decr Podsw
      Led = 0
   End If


'aktualizacja wyświetlania
   Incr Licz
   If Licz >= 12 Then
      Licz = 0
      Actual = 1
   End If

'aktualizacja pid
   If Run = 1 Then
      Incr Pid_inc
      If Pid_inc >= 160 Then
         Pid_inc = 0
         Act_pid = 1
      End If
   Else
      Act_pid = 0
      Pid_inc = 0
   End If

   If Tpiec_err = 0 And T_piec >= 87 Then
      Toggle Buzz
      Buzz_once = 0
      Podsw = 255
   Elseif Buzz_once = 0 And T_piec < 86 Then
      Buzz = 0
      Buzz_once = 1
   End If

Return

End
