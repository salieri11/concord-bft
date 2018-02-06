# VMware Project Athena

Project Athena's goal is to provide an easy-to-deploy blockchain
solution, with stronger guarantees than longest-chain concensus
systems like "proof of work" or "proof of stake", and better
performance than classic Byzantine fault tolerant algorithms like
PBFT.

The project is composed of three components:

 * (SBFT)[vmwathena/sbft] is the consensus engine for Athena

 * (athena)[vmwathena/athena] (this repo) provides a key-value
   abstraction atop SBFT, and an Etherium VM compatibility layer on
   top of that KV storage

 * (helen)[vmathena/helen] is the home of Athena's UI and external API

TODO: implement the equivalent of
P2_Blockchain/Blockchain/basicBlockchain2 and
P2_Blockchain/Blockchan/EtheriumBlockchain here.