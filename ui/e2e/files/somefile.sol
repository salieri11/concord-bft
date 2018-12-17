
pragma solidity ^0.4.18;
// issues:
// 1. Same account can not be used for both buying and selling
// 2. Same account can not be used for buying or selling multiple assets at once
contract SimpleBidder {
   // bidding prices by sellers and buyers
   mapping (address => uint) private sellerPrices;
   mapping (address => uint) private buyerPrices;
   // dynamic array of addresses of all sellers and buyers
   // We can not search or iterate over a mapping, hence we
   // need to maintain all addresses.
   address[] private sellers;
   address[] private buyers;
   // Amounts that can be withdrawn by a seller
   // after successful transaction
   mapping (address => uint) private balances;
   // A match was found between buyer & seller - Transaction is complete
   // The seller should register for this event and should call 'withdraw' function
   // to get his money.
   event TransactionComplete(address seller, address buyer, uint cost);
   event Balance(address adr, uint balance);
   // Find the index of first seller who is willing to sell at or below @price
   function firstSellerAtorBelow(uint price) private view returns (int)  {
      for (uint i = 0; i < sellers.length; i++) {
         if (sellerPrices[sellers[i]] <= price)
            return int(i);
      }
      return -1;
   }
   // Find the index of first buyer who is willing to buy at or above @price
   function firstBuyerAtorAbove(uint price) private view returns (int) {
      for (uint i = 0; i < buyers.length; i++) {
         if (buyerPrices[buyers[i]] >= price)
            return int(i);
      }
      return -1;
   }
   // finds the index of given address in the given array of addresses
   function indexOf(address[] addresses, address adr) private returns (int) {
      for (uint i = 0; i < addresses.length; i++) {
         if (addresses[i] == adr) {
            return int(i);
         }
      }
      return -1;
   }
   function deleteAt(address[] addresses, uint index) private {
      // We can ideally remove element at index 'i' and shift all elements
      // buy 1 position but that would probably require more gas and we don't
      // care about the order anyway so just move last element to 'i'th position
      // and delete last element
      addresses[index] = addresses[addresses.length - 1];
      delete addresses[(addresses.length - 1)];
   }
   function completeTransaction(uint iBuyer, uint iSeller) private {
      // transaction will always happen at sellers price
      // buyer will have to quote equal or more than that price
      address seller = sellers[iSeller];
      address buyer = buyers[iBuyer];
      uint sellPrice = sellerPrices[seller];
      // remove this seller and buyer from address array
      deleteAt(sellers, iSeller);
      deleteAt(buyers, iBuyer);
      // remove seller and buyer from mappings
      sellerPrices[seller] = 0;
      buyerPrices[buyer] = 0;
      // set seller balance (i.e the amount that he can withdraw)
      balances[seller] = sellPrice;
      balances[buyer] -= sellPrice;
      // emit an event to let seller know that he can now withdraw money
      TransactionComplete(seller, buyer, sellPrice);
   }
   function sell(uint amount) public {
      require (amount > 0);
      int iSeller; // we can save some gas by not calling 'indexOf' for new users
      uint sellPrice = amount;
      if (sellerPrices[msg.sender] != 0) {
         // update the selling price for existing bid
         sellPrice += sellerPrices[msg.sender];
         iSeller = indexOf(sellers, msg.sender);
      } else {
         // register a new seller
         sellers.push(msg.sender);
         iSeller = int(sellers.length - 1);
      }
      sellerPrices[msg.sender] = sellPrice;
      // check if any compatible buyer is available
      int iBuyer = firstBuyerAtorAbove(sellPrice);
      if (iBuyer >= 0) //buyer found remove seller and buyer from list
         completeTransaction(uint(iBuyer), uint(iSeller));
   }
   // Withdraws money form buyers account and that money will be sent to
   // seller as soon as a matching seller is found. In the meantime
   // buyer can cancel his bid and request to withdraw that money.
   function buy(uint amount) payable public {
      require (amount > 0);
      require (msg.value >= amount);
      int iBuyer; // we can save some gas by not calling 'indexOf' for new users
      uint buyPrice = amount;
      if (buyerPrices[msg.sender] != 0) {
         buyPrice += buyerPrices[msg.sender];
         iBuyer = indexOf(buyers, msg.sender);
      } else {
         buyers.push(msg.sender);
         iBuyer = int(buyers.length - 1);
      }
      buyerPrices[msg.sender] = buyPrice;
      balances[msg.sender] = buyPrice;
      // check if any compatible seller is available
      int iSeller = firstSellerAtorBelow(buyPrice);
      if (iSeller >= 0)
         completeTransaction(uint(iBuyer), uint(iSeller));
   }
   // Allows the buyer or seller to cancel his bid
   // In case of buyer his money will be transferred back to him
   function cancelBuyerBid() public returns (uint) {
      int iBuyer = indexOf(buyers, msg.sender);
      if (iBuyer >= 0) {
         balances[msg.sender] = buyerPrices[msg.sender];
         buyerPrices[msg.sender] = 0;
         deleteAt(buyers, uint(iBuyer));
      }
      return balances[msg.sender];
   }
   // In case of seller the entry is simply deleted from state since
   // seller never transferred ether to contract
   function cancelSellerBid() public returns (uint) {
      int iSeller = indexOf(sellers, msg.sender);
      if (iSeller >= 0) {
         sellerPrices[msg.sender] = 0;
         deleteAt(sellers, uint(iSeller));
      }
      return balances[msg.sender];
   }
   // Allows the buyer or seller
   function withdraw(uint amount) public {
      require (amount > 0);
      require (balances[msg.sender] >= amount);
      balances[msg.sender] -= amount;
      msg.sender.transfer(amount);
   }
   function getBalance(address adr) view public returns (uint) {
       Balance(adr, balances[adr]);
       return balances[adr];
   }
}
  