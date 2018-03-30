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
</style>"""


def createResultHeader(results, testCount, passCount, failCount):
   suiteName = list(results.keys())[0]   
   suiteResult = results[suiteName]["result"]
   
   html = "<html>\n{}\n<body>\n".format(styles)
   html += "Suite: {}<br>\n".format(suiteName)
   html += "Result: {}<br><br>\n\n".format(suiteResult)
   html += "Tests: {}<br>\nPass: {}<br>\nFail: {}<br><br>\n\n" \
           .format(testCount,
                   passCount,
                   failCount)
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
      "info": "Log: /tmp/CoreVMTests_20180323_1406_tdcdm69z/test_logs/sha3_0",
      "result": "PASS"
   }
   returns a string of HTML to add a row to a table.
   '''
   row = None

   if result["result"] == "PASS":
      row = "<tr>"
   else:
      row = "<tr class='fail'>"

   row += "<td>{}</td><td>{}</td><td>{}</td></tr>\n".format(testName,
                                                            result["result"],
                                                            result["info"])
   return row
