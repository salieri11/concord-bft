import re

styles = """<style>
table {
       border-collapse: collapse;
   }

table, td, th {
       border: 1px solid black;
   }

.fail{
      background-color:#ffcfcf;
   }

.skipped{
      background-color:#ffffcf;
   }
</style>"""


def createResultHeader(results, testCount, passCount, failCount, skippedCount):
   suiteName = list(results.keys())[0]   
   suiteResult = results[suiteName]["result"]
   
   html = "<html>\n{}\n<body>\n".format(styles)
   html += "Suite: {}<br>\n".format(suiteName)
   html += "Result: {}<br><br>\n\n".format(suiteResult)
   html += "Tests: {}<br>\nPass: {}<br>\nFail: {}<br>\nUnintentionally " \
           "Skipped: {}<br><br>\n\n" \
           .format(testCount,
                   passCount,
                   failCount,
                   skippedCount)
   return html

def createResultTable(results):
   suiteName = list(results.keys())[0]   
   testCases = results[suiteName]["tests"]
   htmlTable = "<table>\n"

   for test in testCases:
      htmlTable += createHtmlRow(test, testCases[test])

   htmlTable += "</table>"
   return htmlTable

def createHtmlRow(testName, result):
   '''
   Given a test case dictionary such as:
   "sha3_0": {
      "info": "Log: /tmp/EthCoreVmTests_20180323_1406_tdcdm69z/test_logs/sha3_0",
      "result": "PASS"
   }
   returns a string of HTML to add a row to a table.
   '''
   row = None

   if result["result"] == "PASS":
      row = "<tr>"
   elif result["result"] == "FAIL":
      row = "<tr class='fail'>"
   else:
      row = "<tr class='skipped'>"

   htmlFormattedInfo = result["info"].replace("\n", "<br>")
   htmlFormattedInfo = htmlFormattedInfo.replace("\\n", "<br>")
   htmlFormattedInfo = re.sub(r" (?!href=)", "&nbsp;", htmlFormattedInfo)
   row += "<td>{}</td><td>{}</td><td>{}</td></tr>\n".format(testName,
                                                            result["result"],
                                                            htmlFormattedInfo)
   return row

def createHtmlFooter():
   return "</body></html>"
