pragma solidity ^0.5.2;

contract Simple {
    event Event(int indexed value);

    function foo(int a) public {
        emit Event(a);
    }
}
