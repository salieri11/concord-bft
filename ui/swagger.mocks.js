const fs = require('fs');
const SwaggerTooth = require('swaggertooth').SwaggerTooth;
const config = SwaggerTooth.configFile('swagger.mocks.conf.json');

new SwaggerTooth(config).generateExamples();

console.log(`Generated swagger samples requests & responses from paths...`); 
console.log(``);
const result = JSON.parse(fs.readFileSync(config.outputFolderPath + '/generation.info.json'));
console.log(`Source Swagger:  '${config.sourceSwaggerPath}'`); 
console.log(`Output folder :  '${config.outputFolderPath}'`); 
console.log(`Total API Paths:  ${result.info.totalPaths}, Files: ${result.info.totalItems}`);
console.log(`Requests: ${result.info.totalRequests}, Responses: ${result.info.totalResponses}`);
console.log(``);
