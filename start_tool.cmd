@ECHO OFF
ECHO .
ECHO                                    (.)_(.)                                                         
ECHO                                 _ (   _   ) _                                                
ECHO                                / \/`-----'\/ \                                                
ECHO   ( )_( )      ( )_( )       __\ ( (     ) ) /__       ( )_( )      ( )_( )
ECHO   (='.'=)      (='.'=)       )   /\ \._./ /\   (       (='.'=)      (='.'=)
ECHO   (^^)_(^^)      (^^)_(^^)        )_/ /^|\   /^|\ \_(        (^^)_(^^)      (^^)_(^^)

::@echo off
:: Script om de SPEK te STARTEN.
 

:: Bepaal locatie van script
SET _me=%~n0
SET _parent=%~dp0
 
:: Configuratie
SET _RVersieKort=4.4
SET _RVersieLang=4.4.0
 
:: Locatie van centrale R installatie
SET _RCentraal=R:\\%_RVersieKort%\R-%_RVersieLang%\bin
:: Geef uitvoer locatie door aan RScript/Rsessie
SET R_USER=%_parent%
 
:: Maak map voor log bestanden
mkdir "%_parent%log" 2>nul
ECHO %_Rcentraal%
::@ECHO OFF 
:: Start applicatie
Rscript.exe %_parent%\start.R
PAUSE 
