param
(
  [string]$excelbestand
)
$excel = new-object -comobject Excel.Application
$excel.DisplayAlerts = $false
$wb = $excel.Workbooks.Open($excelbestand)
$wb.Save()
$wb.Close()

