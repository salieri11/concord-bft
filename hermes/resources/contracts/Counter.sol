pragma solidity ^0.4.22;

contract Counter {
    int private count;

    constructor() public payable {
        count = 0;
    }

    function incrementCounter(int x) public payable {
        count += x;
    }

    function decrementCounter(int x) public {
        count -= x;
    }

    function getCount() public constant returns (int) {
        return count;
    }

    function() external payable {
        count = 1000;
    }
}
