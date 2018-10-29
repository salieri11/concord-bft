pragma solidity ^0.4.19;

contract DummyContract {
   function returnTrue() public pure returns (bool) {
       return true;
   }
}

contract HelloWorld {
   function hello() public pure returns (string) {
       return "Hello, World!";
   }
}