
To Do List
==========

### Documentation

* README: review IB Controller username/password instructions, review latest IB Controller release
* Haddocks markup

### Defects

* Requests-Simple: thread blocked indefinitely in an STM transaction
* Build on GHC 7.8.x
* Test pico issue on GHC 7.8.x

### New features

* IB Controller launcher (TWS and Gateway). Test IB Gateway app
* Add a connection timeout (secs) config setting?
* Simple interface (connection object)?
* Remaining IB requests and responses (summarise here from list in Enum.hs)
* Logging
* Contract database, automated update

### Build

* Cabal file: standardise package dependency format to best practice

### Design

* Refactor Edge to ListT (use state for partial parse)
* Async/exception handling
* Config read only (vs in 'mutable' state at present)?
* Some data unncecessary in IB connection state (e.g. accounts, next valid id, etc?) 
* BASE currency (in AccountValue msg) solution

### Test suite 

* Unit tests
* Property tests
* Benchmarks

### Sample apps

* Requests-Simple: rename, add all requests
* Requests-Service: rename, change to be a basic algo trader (e.g. moving average crossover), separate project?
* Shell: IB shell (build into API?) - convert to IB monad, add full request support, single command invocation
* Capture (new): historical/streaming market data to file/db

### Connection

* Add remaining conditions to ensure state machine is robust (e.g. can't process start unless pending) and invalid commands/condtions are logged
* ServiceCommand: implement pause, resume
* Socket service status messages: implement handlers
* ibsNextRequestId: add? if so, implment update logic
* Check client server version <= server version
* Error handler for error responses from IB (see IB API docs for error codes)
* Error response messages - see EClientSocket.java e.g. not connected, cannot send (also cannot create which currently only logs a message)

### Monadic interface

* Disconnect
* Simplify handling of request/order id's and server version in request messages

### Builder

* Contract builders: stock, forex, option
* Order builders: stop/limit/stoplimit orders, attached orders (stop loss, profit target, oco stop and profit target)

### Data 

* Consider using Text (github issue #1)
* IBOrder: into multiple data types
* IBOrder: maybe's for bools
* IBOrder: decomposition may allow a single maybe for related fields
* Consistent use of IB prefix for data type names (e.g. IBRequest and IBResponse constructors and fields)
* Use Data.Default for base instances (vs newIBContract, etc.)

### Requests

* Create message functions to return Either with string/typed failure reason? Or use error "<reason>"?
* Use typing to prevent some of the error conditions (e.g. bar size)
* Lazy vs strict bytestring (github issue #2)
* Remove req historical data option to specify date format - just use one of the two options internally

### Parsing

* FromIB class for parsing?
* Review fields for possible signed / blank / default parsing requirement
* IBContract: complete refactoring to use field level parsers
* TickPrice: parse also generates a TickSize but latter seems to also stream separately - confirm/remove duplication? Have commented out TickSize generation from TickPrice parser.
* Add error message to fail "" (and empty?) statements

### Socket service

* Move to another library (e.g mvc-service)
* Complete model (e.g. checking and updating state)
* Tidy up Connection code, consistent use of STM vs IO
* Streamline verbose code e.g. atomically
* Secure SSL option
* Tests and benchmarks

### MVC Service

* Lenses for Service
* Service resume command needed (vs reuse service start in paused status)























