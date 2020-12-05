https://travis-ci.com/MikeSidorochkin/ABAP-Simple-pRFC-Framework.svg?branch=main
# ABAP Simple pRFC Framework

Parallel-processing is implemented with a special variant of [asynchonous RFC](https://help.sap.com/viewer/7bfe8cdcfbb040dcb6702dada8c3e2f0/1709%20000/en-US/4d909309eba36e73e10000000a15822b.html). 

ABAP requirements (see also the online documentation for the CALL FUNCTION STARTING NEW TASK DESTINATION IN GROUP keyword):

- The function module that you call must be marked as externally callable. This attribute is specified in the Remote function call supported field in the function module definition (transaction SE37)).
- The called function module may not include a function call to the destination "BACK".
- The calling program should not change to a new internal session after making an asynchronous RFC call. That is, you should not use SUBMIT or CALL TRANSACTION in such a report after using CALL FUNCTION STARTING NEW TASK.
- You cannot use the CALL FUNCTION STARTING NEW TASK DESTINATION IN GROUP keyword to start external programs.

Demo program: [ZBC_ASYNC_DEMO](https://github.com/MikeSidorochkin/ABAP-Simple-pRFC-Framework/blob/main/zbc_async_demo.prog.abap).
