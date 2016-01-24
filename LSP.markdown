LSP Protocol
============


Protocol Overview
-----------------

    Initiator                                         Receiver


    Hello      ------------------------------------>
               <------------------------------------  Hello

    Data       <----------------------------------->  Data

Handshake
---------

The `Hello` message is defined as such:

    struct Hello
    { random     : opaque256
    , version    : u16
    , hello-data : HelloData(version)
    }

The initiator will send a Hello packet with the expected version, and a random
field, and depending on the version, cryptographic materials to create a shared state
and be cryptographically identified (i.e. a key and signature).

The receiver will check the version is known and allowed, and that
the signature of the initiator's hello message match the expected key.


Data Message
------------

Data message is a length prefix opaque data. The length is reported in bytes, not bits.

    struct Data
    { length : u32
    , data   : opaque<length>
    }

Version 1
---------

In a nutshell Version 1, the initiator and receiver exchanges Curve25519
keys, which allow to establish a shared cryptographic state. Identities
of both parties are assured by signing the hello message and the hello data
message by a Ed25519 key (node-key).

The `HelloData` is defined

    struct HelloData(1)
    { dh-public-key   : opaque256 -- Curve25519 public key
    , node-public-key : opaque256 -- ED25519 public key
    , node-signature  : opaque512 -- ED25519 signature
    }

The node signature in the version 1 instance is computed on every bytes of data
in the message preceding the signature value.

     sign(node-secret-key, random | version | dh-public-key | node-public-key)

The signature is used to prove the authenticity of the `Hello` message,
while also giving an identifier `node-public-key` of the owner of the message.

When the other side `Hello` message has been authenticated, both parties can
establish the master-secret using one of this formula:

    pre-master-secret = Curve25519-ECDH(initiator-public-key, receiver-secret-key)
    pre-master-secret = Curve25519-ECDH(receiver-public-key, initiator-secret-key)


Cryptographic computation
-------------------------

The MasterSecret is generated from the pre-master-secret using :

    PRF (pre-master-secret, "master secret" | client-random | server-random)

The PRF used:

    PRF(secret, seed) = HMAC-SHA512(secret, A(1) | seed)
                      | HMAC-SHA512(secret, A(2) | seed)
                      | HMAC-SHA512(secret, A(3) | seed)
                      | ...

    A(0) = seed
    A(i) = HMAC-SHA512(secret, A(i-1))

The Master Secret is then used to generate the TX and RX record cryptographic state.

Appendix - Presentation Language
--------------------------------

### Comment

Comment start by the `--` and finish implicitly at the end of the line.

     Type -- This is a comment

### Integer

Represent integer with fixed size, either in unsigned or signed fashion.
The integer are always in little endian format.

    u<size>
    i<size>

Example:

    u16 -- 16 bits little endian
    i32 -- 32 bits little endian

### Opaque

Opaque represent an array of byte of specific size in bits.
Only multiples of 8 bits are supported.

    opaque<size>

Example:

    opaque256 -- 256 bits (32 bytes) of opaque data

### Parametrized Type

The following definition

    DataStructure(Parameter)

represent a type defined by a parameter either provided by a previous
field or an external value.

Example:

    HelloData(version)

### Struct

Struct give the ability to define a sequence of data (product type) following
each other. Each comma separated field have a label used for descriptive purpose.

Each values is serialized according to their types, one after another. The labels
have no implication on the serialized data.

    struct Name
    { field1  : type
    , field2  : type
    ...
    , fieldN  : type
    }

### Misc

The `|` operator represent concatenation at the byte level


Appendix - Comparison to TLS
----------------------------

* Simple handshake protocol
* Simple record layer
* Simple presentation language
* No embedded web of trust or PKI (no ASN.1, no X509)
* Only use modern cryptography
* No Extensions
* No cipher choices: protocol version defines cipher
* No key exchanges choices
