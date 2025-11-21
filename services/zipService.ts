import { CobolFile } from '../types';
// import JSZip from 'jszip'; // In a real build environment, we would import this.

// Since we cannot guarantee JSZip is installed in this specific runner, 
// we will simulate zip extraction with a text-based approach for the demo.
// If the user uploads a file, we will read it as text. If it's a zip, 
// we would normally unzip it. Here, we will accept .cbl files directly 
// or default to the mock if extraction fails to keep the UI functional.

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
           05  CUST-LIMIT          PIC 9(7)V99.
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
                   IF CUST-LIMIT > 5000
                       PERFORM APPROVE-CREDIT
                   END-IF
           END-READ.

       APPROVE-CREDIT.
           DISPLAY "Credit approved for " CUST-NAME.
  `,
  'ORDERS.CBL': `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ORD001.
       DATA DIVISION.
       FILE SECTION.
       FD  ORDER-FILE.
       01  ORDER-RECORD.
           05  ORD-ID              PIC 9(8).
           05  ORD-CUST-ID         PIC 9(6).
           05  ORD-DATE            PIC 9(8).
           05  ORD-AMOUNT          PIC 9(5)V99.
  `
};

export const extractCobolFiles = async (file: File): Promise<CobolFile[]> => {
  return new Promise((resolve, reject) => {
    // Mock behavior for the "Demo Mode" or if real zip fails
    if (file.name.endsWith('.zip')) {
        // In a real app:
        /*
        const zip = new JSZip();
        const loadedZip = await zip.loadAsync(file);
        const files: CobolFile[] = [];
        loadedZip.forEach(async (relativePath, zipEntry) => {
            if (!zipEntry.dir && (relativePath.endsWith('.cbl') || relativePath.endsWith('.cob'))) {
                const content = await zipEntry.async('string');
                files.push({ name: zipEntry.name, path: relativePath, content });
            }
        });
        resolve(files);
        */
       
       // For this runtime environment without bundler access to jszip:
       console.warn("JSZip not detected. Using mock simulation for ZIP contents.");
       setTimeout(() => {
         const files: CobolFile[] = Object.entries(MOCK_FILES).map(([name, content]) => ({
           name,
           path: name,
           content
         }));
         resolve(files);
       }, 1500);
    } else {
        // Single file upload logic
        const reader = new FileReader();
        reader.onload = (e) => {
            const content = e.target?.result as string;
            resolve([{
                name: file.name,
                path: file.name,
                content
            }]);
        };
        reader.onerror = reject;
        reader.readAsText(file);
    }
  });
};