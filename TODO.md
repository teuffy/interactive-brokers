
To Do List
==========

### Documentation

* Haddocks markup

### Defects

* Build on GHC 7.8.x
* Test pico issue on GHC 7.8.x

### New features

* IB Controller launcher (TWS and Gateway). Test IB Gateway app
* Add a cfgConnectionTimeOut setting?
* Command terminal (e.g. use Haskeline)
* Monadic interface
* Simple interface (connection object)
* Remaining IB requests and responses (summarise here from list in Enum.hs)
* Higher level API for requests (e.g. create order, including attached orders, oco orders, etc.)

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

### Data 

* Consider using Text (github issue #1)
* IBOrder: into multiple data types
* IBOrder: maybe's for bools
* IBOrder: decomposition may allow a single maybe for related fields
* Consistent use of IB prefix for data type names (e.g. IBRequest and IBResponse constructors and fields)
* Use Data.Default for base instances (vs newIBContract, etc.)

### Event Processor

* Auto-start option
* ServiceCommand: implement start, pause, resume
* ibsNextRequestId: add? if so, implment update logic
* ibsNextOrderId: retain? if so, implement remaining update logic
* Check client server version <= server version
* Error handler for error responses from IB (see IB API docs for error codes)
* Error response messages - see EClientSocket.java e.g. not connected, cannot send (also cannot create which currently only logs a message)

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























