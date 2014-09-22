
To Do List
==========

### Documentation

* README: add list of supported and unsupported messages
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

### Design

* Refactor Edge to ListT (use state for partial parse)
* Async housekeeping audit
* Config read only (vs in 'mutable' state at present)?
* Some data unncecessary in IB connection state (e.g. accounts, next valid id, etc?) 
* BASE currency (in AccountValue msg) solution - was in currency enum in hq.data.currency

### Test suite 

* Unit tests
* Property tests
* Benchmarks

### Data 

* Order id's to Int (e.g. IBRequest/PlaceOrder.reqOrderId)
* More typing (enum,utctime,etc.) instead of strings/bytestrings (e.g. in contract)
* Principled consistent use of bytestring/string/text (github issue #1)
* Scientific/Decimal solution for decimal numbers (github issue #3)
* IBOrder: into multiple data types
* IBOrder: maybe's for bools
* IBOrder: decomposition may allow a single maybe for related fields
* Consistent use of IB prefix for data type names (e.g. IBRequest and IBResponse constructors and fields)

### Event Processor

* Auto-start option
* Debug option
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

### Parsing

* Use thyme library?
* Use ParseTime intances?
* FromIB class for parsing?
* Review fields for possible signed / blank / default parsing requirement
* IBContract: complete refactoring to use field level parsers
* TickPrice: parse also generates a TickSize but latter seems to also stream separately - confirm/remove duplication? Have commented out TickSize generation from TickPrice parser.
* Add error message to fail "" statements

### Socket service

* Move to another library (e.g mvc-service)
* Complete model (e.g. checking and updating state)
* Tidy up Connection code, consistent use of STM vs IO
* Streamline verbose code e.g. atomically
* Secure SSL option
* Tests and benchmarks























