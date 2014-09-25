# Interactive Brokers API

This library provides a Haskell wrapper for the Interactive Brokers API. It is based on [version 9.70 of the IB Java API](http://interactivebrokers.github.io/downloads/twsapi_macunix.970.01.jar). The latest version at the time of writing is 9.72. See [the IB API home page](https://www.interactivebrokers.com/en/index.php?f=5041) for up-to-date information.

## Status

* Work in progress (see TODO.md for planned enhancements).
* Working sample apps that demonstrate currently supported messages
* A test suite has not yet been developed
* Performance analysis has not yet been conducted

## API Coverage

The following messages are currently supported.

### Requests

* ReqMktData
* CancelMktData
* PlaceOrder
* CancelOrder
* ReqOpenOrders
* ReqAccountData
* ReqExecutions
* ReqIds
* ReqContractData
* ReqAutoOpenOrders
* ReqAllOpenOrders
* ReqManagedAccounts
* ReqHistoricalData
* CancelHistoricalData
* ReqCurrentTime
* ReqRealTimeBars
* CancelRealTimeBars
* ReqGlobalCancel
* ReqMarketDataType
* ReqPositions
* ReqAccountSummary
* CancelAccountSummary
* CancelPositions

### Responses

* TickPrice
* TickSize
* OrderStatus
* ErrorMessage
* OpenOrder
* AccountValue
* PortfolioValue
* AccountUpdateTime
* NextValidId
* ContractData
* ExecutionData
* ManagedAccounts
* HistoricalData
* TickGeneric
* TickString
* CurrentTime
* RealTimeBar
* ContractDataEnd
* OpenOrderEnd
* ExecutionDataEnd
* TickSnapshotEnd
* MarketDataType
* CommissionReport
* Position
* PositionEnd
* AccountSummary
* AccountSummaryEnd

## Setup

Tested on GHC 7.6.3:

    git clone https://github.com/cmahon/interactive-brokers.git
    git clone https://github.com/cmahon/mvc-service.git
    cd interactive-brokers
    cabal sandbox init
    cabal sandbox add-source ../mvc-service
    cabal install --only-dependencies -p
    cabal configure -p
    cabal build
    cabal install
    
With [IB TWS](#ib-tws) running, start the example application which issues a series of requests to the IB API and sends the responses to stdout:

    cabal run requests-simple
    cabal run requests-service

## IB Installation (OS X)

### IB TWS

The IB API can be accessed via the full desktop client (TWS - Trader Workstation) or the gateway client. I have only used TWS to date but will try the gateway soon - from what I can tell it only offers a restricted subset of the native IB API (e.g. orders may not be supported) but supports the full API via FIX.

Create a folder for TWS.

    mkdir ~/IBJts

Download and run the [latest TWS installer for OS X](https://download2.interactivebrokers.com/download/TWSX_install_latest.pkg) and run it. Copy all files from the Contents/Resources/Java subfolder of the newly created Trader_WorkStation_X.app package to ~/IBJts. For example:

    cp -r /Users/<Username>/Applications/Trader_WorkStation_X.app/Contents/Resources/Java ~/IBJts

Change to the IBJts folder:

    cd ~/IBJts

Run TWS:

    java -cp jts.jar:total.jar -Xmx512M -XX:MaxPermSize=128M jclient.LoginFrame .

Create a shell file to launch TWS and save as 'ibtws':

    #!/bin/sh
    cd ~/IBjts
    java -cp jts.jar:total.jar -Xmx512M -XX:MaxPermSize=128M jclient.LoginFrame . 

Make ibtws executable

    chmod +x ibtws

### IBController

Download the [latest IBController code](https://github.com/ib-controller/ib-controller) and extract the archive in your home folder. Change to the newly created IBControllerVX-XX-X folder.

    cp sampleIBControllerStart.sh ibc
    chmod +x ibc

Edit ibc. Mine looks like this (note the commented-out username and password):

    #!/bin/bash
    TWSUSERID=<IB username>
    TWSPASSWORD=<IB password>
    IBCDIR=/Users/Chris/IBController/
    IBCINI=/Users/Chris/IBController/IBController.ini
    TWSDIR=/Users/Chris/IBJts
    TWSCP=jts.jar:total.jar
    JAVAOPTS="-Xmx512M -XX:MaxPermSize=128M" 
    pushd $TWSDIR
    java -cp  $TWSCP:$IBCDIR/IBController.jar $JAVAOPTS ibcontroller.IBController $IBCINI $TWSUSERID $TWSPASSWORD
    popd

Edit the following entries in IBController.ini.

    IbLoginId=<IB username>

    IbPassword=<IB password>

    IbDir=/Users/Chris/IBJts

### Testing

Make ibtws and ibc accessible e.g. via copying to a folder in your PATH.

Running ibtws should launch the TWS client and request your login details:

    ibtws

Running ibc should launch IB TWS and login automatically:

    ibc

### Enabling the IB API in TWS

Activate via menu / setting dialog:

Edit - Global configuration - API - Settings - Enable ActiveX and Socket Clients

