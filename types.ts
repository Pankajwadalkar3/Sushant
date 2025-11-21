export enum TransformationTarget {
  DOTNET = 'DOTNET',
  JAVA = 'JAVA'
}

export enum AppState {
  UPLOAD = 'UPLOAD',
  ANALYZING = 'ANALYZING',
  REVIEW = 'REVIEW',
  TRANSFORMING = 'TRANSFORMING',
  COMPLETE = 'COMPLETE'
}

export interface CobolFile {
  name: string;
  content: string;
  path: string;
}

export interface Datapoint {
  category: 'ENTITY' | 'LOGIC' | 'DATABASE' | 'UI' | 'DEPENDENCY';
  name: string;
  description: string;
  confidence: number; // 0-100
  sourceLines?: string;
}

export interface AnalysisResult {
  programId: string;
  summary: string;
  datapoints: Datapoint[];
  complexity: 'LOW' | 'MEDIUM' | 'HIGH';
}

export interface GeneratedFile {
  path: string; // e.g., "src/Domain/Models/Customer.cs"
  content: string;
  language: 'csharp' | 'java' | 'json' | 'xml';
}

export interface ProjectStructure {
  files: GeneratedFile[];
}

export const MOCK_FILES = {
  'CUSTOMER.CBL': `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUST001.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE ASSIGN TO "CUST.DAT"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS CUST-ID.
       DATA DIVISION.
       FILE SECTION.
       FD  CUSTOMER-FILE.
       01  CUSTOMER-RECORD.
           05  CUST-ID             PIC 9(6).
           05  CUST-NAME           PIC X(30).
           05  CUST-EMAIL          PIC X(50).
           05  CUST-BALANCE        PIC 9(7)V99.
       WORKING-STORAGE SECTION.
       01  WS-FLAGS.
           05  END-OF-FILE         PIC X VALUE 'N'.
               88  EOF             VALUE 'Y'.
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           OPEN I-O CUSTOMER-FILE
           PERFORM PROCESS-CUSTOMERS UNTIL EOF
           CLOSE CUSTOMER-FILE
           STOP RUN.
       
       PROCESS-CUSTOMERS.
           READ CUSTOMER-FILE NEXT RECORD
               AT END MOVE 'Y' TO END-OF-FILE
               NOT AT END
                   IF CUST-BALANCE > 1000
                       PERFORM SEND-OFFER
                   END-IF
           END-READ.

       SEND-OFFER.
           DISPLAY "Sending offer to " CUST-NAME.
  `
};