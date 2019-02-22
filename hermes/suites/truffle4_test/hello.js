var HelloWorld = artifacts.require("HelloWorld");

module.exports = async function(callback) {
    HelloWorld.at(HelloWorld.address).hello().then(msg => {
        console.log(msg);
    });
}
