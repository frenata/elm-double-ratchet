### Elm Double Ratchet

This library implements Signal's [Double Ratchet](https://signal.org/docs/specifications/doubleratchet/) protocol to provide end-to-end encryption between two parties given a shared secret key.

It's written in Elm to make reasoning about the correctness of the functions easy but still allow for easy deployment to the browser. [Encryption in the browser](http://secushare.org/end2end#sec-1) may not be a great idea in the abstract, but it was interesting challenge. A simple web application could use this protocol to run from a user's harddrive directly.


#### Usage

A full demo application will be forthcoming, but a very simple demo of the current state is available in the [demo](https://github.com/frenata/elm-double-ratchet/tree/master/demo) directory.

Imagine that two users want to communicate: Alice and Bob.

1. Bob generates a pair of keys.
2. Bob sends his public key to Alice.
3. Alice runs "Init" with Bob's public key.
4. Alice, preparing to send, ratchets her Send chain.
5. Alice types a plaintext message, then encrypts the message into a ciphertext.
6. Alice sends her public key and her ciphertext to Bob.
7. Bob runs "Receive FK" on Alice's public key.
8. Bob, preparing to recieve, ratchets his Recieve chain.
9. Bob decrypts Alice's ciphertext.

At this point, steps 4-9 can be repeated as desired in either direction. Public keys are only sent and received when the "polarity" of the conversation changes, ie. when Bob sends a message to Alice rather than Alice to Bob. The Send and Recieve chains should be ratcheted before every encryption/decryption, respectively.

#### Short Term Goals

* automatic symmetric ratcheting when encrypting/decrypting
* authentication
* concatenation of cipher text with a header
* replace curve with 25519

#### Long Term Goals

* replace SJCL with Elm primitive crypto
* encrypted headers
