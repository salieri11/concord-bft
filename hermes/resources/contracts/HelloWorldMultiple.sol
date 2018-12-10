pragma solidity ^0.4.19 || ^0.5;

contract DummyContract {
   function returnTrue() public pure returns (bool) {
       return true;
   }
}

contract HelloWorld {
   function hello() public pure returns (string memory) {
       return "Hello, World!";
   }
}