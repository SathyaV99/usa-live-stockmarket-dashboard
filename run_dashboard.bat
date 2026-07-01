@echo off
echo Starting USA Live Stockmarket Dashboard...
"D:\R-4.6.1\bin\Rscript.exe" -e "shiny::runApp('c:/Users/venka/Documents/usa-live-stockmarket-dashboard', launch.browser=TRUE)"
pause
