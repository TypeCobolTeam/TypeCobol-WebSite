---
title: Type Samples
---

# Types
Here's a sample type created with TypeCobol.
```cobol
       01 DateJulian TYPEDEF STRICT PUBLIC.
         10 YYYY PIC 9(04).
         10 DDD  PIC 9(03).    
       01 DateDB2 TYPEDEF STRICT PUBLIC.
         10  YYYY   PIC 9(04).
         10  filler PIC X value '-'.
         10  MM     PIC 9(02).
         10  filler PIC X value '-'.
         10  DD     PIC 9(02).
       01 Time4 TYPEDEF STRICT PUBLIC.
         10 Hour      pic 9(02).
         10 Minute    pic 9(02).
       01 Time6 TYPEDEF STRICT PUBLIC.
         10 Hour      pic 9(02).
         10 Minute    pic 9(02).
         10 Second    pic 9(02).
       01 Time8 TYPEDEF STRICT PUBLIC.
         10 Hour      pic 9(02).
         10 Minute    pic 9(02).
         10 Second    pic 9(02).
         10 Ms        pic 9(02).
       01 TimeDB2 TYPEDEF STRICT PUBLIC.
           05 Hour    pic 9(02).
           05 filler  pic X value '.'.
           05 Minute  pic 9(02).
           05 filler  pic X value '.'.
           05 Second  pic 9(02).
       01  TimeStampDB2 TYPEDEF STRICT PUBLIC.
           05  DateDB2             type DateDB2.
           05  filler              pic X value '-'.
           05  TimeDB2             type TimeDB2.
           05  filler              pic X value '.'.
           05  Nanosecond          pic 9(06).
```