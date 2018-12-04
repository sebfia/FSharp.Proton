### 0.1.0 - 23.03.2017
* First release of Proton.
### 0.2.0 - 27.03.2017
* Caching reflection for performance improvement.
### 0.3.0 - 07.04.2017
* Added derializer for FSharp Option type and ability to serialize types implementing ToProto within a ToProto function of another type.
### 0.5.0 - 23.04.2017
* Added message ability to encode closed messages with Base128 length prefix to store several messages in a file or serialize several messages to a network stream.
### 0.5.6 - 24.04.2017
* Extended message module to append multiple messages and fixed serialization of embedded subtypes implementing ToProto and FromProto
### 0.5.7 - 24.04.2017
* Added writer for single byte.
### 0.6.0 - 16.10.2017
* Added function to get single message from byte array.
### 0.6.5 - 01.11.2017
* Added function to serialize single message to byte array. Improved performance on message functions
### 0.6.6 - 04.12.2018
* Upgraded to latest protobuf-net and modernized codebase.