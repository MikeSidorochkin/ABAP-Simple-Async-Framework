# ABAP Simple pRFC Framework

This framework is designed to parallelize data processing in ABAP.

Parallel-processing is implemented with a special variant of [asynchonous RFC](https://help.sap.com/viewer/7bfe8cdcfbb040dcb6702dada8c3e2f0/1709%20000/en-US/4d909309eba36e73e10000000a15822b.html). It is important that you use only the correct variant for your own parallel processing applications: the CALL FUNCTION STARTING NEW TASK DESTINATION IN GROUP keyword. Using other variants of asynchronous RFC circumvents the built-in safeguards in the correct keyword, and can bring your system to its knees.

See [ZBC_ASYNC_DEMO](https://github.com/MikeSidorochkin/ABAP-Simple-pRFC-Framework/blob/main/zbc_async_demo.prog.abap) report.
