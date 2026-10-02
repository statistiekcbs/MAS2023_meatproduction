@ECHO off
pushd "%~dp0"
ECHO Hier kun je de Spek tool bijwerken gebaseerd op veranderingen in de git repository met remote:
ECHO.
ECHO.
call git remote -v
ECHO.
ECHO.
ECHO Dit is de output van git status:
call git status
ECHO.
ECHO.
SET update_type_branch=b
SET update_type_versie=v
ECHO Je kunt het tool bijwerken door de laatste wijzigingen van een specifieke branch of van een release versie (tag) te krijgen. Wat zou je willen doen? 
ECHO.
ECHO.
SET /p "update_type=Typ %update_type_branch% om een branch te gebruiken of %update_type_versie% om een versie te gebruiken:"
SET Correct=0
SET branch=NULL
:while
IF %Correct% == 0 (
	IF %update_type% == %update_type_branch% (
		SET Correct=1
		GOTO :branch_update
	) ELSE IF %update_type% == %update_type_versie% (
		SET Correct=1
		GOTO :versie_update
	) ELSE (
		SET /p "update_type=Fout. Typ %update_type_branch% of %update_type_versie%:"
		SET Correct=0
		GOTO :while
	)
)
:branch_update
ECHO.
ECHO.
ECHO Je hebt gekozen voor een branch. Dit zijn de beschikbare branches:
call git fetch
call git branch --all
ECHO.
SET /p "branch=Typ nu de naam van de branch die je wilt gebruiken (aanbevolen wordt de main branch):"
call git switch %branch%
call git pull
GOTO :end
:versie_update
ECHO.
ECHO.
ECHO Je hebt gekozen voor een versie. Dit zijn de beschikbare versies:
call git fetch --tags
call git tag -l
SET /p "versie=Typ nu de naam van de versie die je wilt gebruiken:"
call git checkout %versie%
GOTO :end
:end
PAUSE
