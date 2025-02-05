import documentai_helper as helper
from google.cloud import storage
from datetime import datetime
import pandas as pd
from time import sleep
import os

# Suppress logging warnings
os.environ["GRPC_VERBOSITY"] = "ERROR"
os.environ["GLOG_minloglevel"] = "2"
os.environ["GRPC_ENABLE_FORK_SUPPORT"] = "1"

# initializing global constants for processor (DON'T CHANGE)
project_id = "head-tax-ci44"
location = "us"
processor_id = "7db1369f50f3a46b"
mime_type = "image/jpeg"
processor_version_id = "pretrained-foundation-model-v1.3-2024-08-31"
#processor_version_id = "71a5ef1c133c5096"

# path constants
bucket = "ci44_data"
out_prefix = '/Users/amykim/Princeton Dropbox/Amy Kim/head_tax/head_tax_data/cleaned/ci44/'
#out_suffix = 'out_basemodel_' + datetime.now().strftime("%b%d") + '.csv'
out_suffix = 'out_basemodel_' + 'Jan29' + '.csv'
operationinfo_path = out_prefix + 'operationinfo.csv'

# script globals
foldername = "test5" #CHANGE
intfolder = "" #should be empty string unless testing (then should end with /)
startreel = 16179 #set to 0 if testing, 16164 if running from scratch # LAST PROCESSED FOLDER (write): 16178
endreel = 16184
LISTLROs = True 
PROCESS = "read" #options: full, write, read
waittime = 60*5 #time to wait between checking if less than 5 active batch process requests

## WORKFLOW
# list ongoing operations
if LISTLROs: 
    helper.list_operations(project_id=project_id, location="us", operations_filter="TYPE=BATCH_PROCESS_DOCUMENTS AND STATE=RUNNING")

# else generate list of in/out uris
storage_client = storage.Client()
if startreel < 16164: #if not looking through multiple reel folders
    folder_roots = [foldername]
else:
    folder_roots = ["t" + str(i) for i in range(startreel, endreel + 1)]

file_in_uris = ["gs://" + bucket + "/raw/" + intfolder + folder + "/" for folder in folder_roots]
file_out_uris = ["gs://" + bucket + "/clean/" + intfolder + folder + "/" for folder in folder_roots] 

nfiles = sum([len(list(storage_client.list_blobs(bucket, prefix = "raw/" + intfolder + folder + "/"))) for folder in folder_roots])

# checking inputs
if PROCESS == "full":
    process_info = "submit each folder as a batch process request, wait for each operation to finish, and save each output as a df"
elif PROCESS == "write":
    process_info = "submit each folder as a batch process request and save each operation in operationinfo_path without waiting for them to finish"
elif PROCESS == "read":
    process_info = "read the output of the most recent batch process request corresponding to each folder and save each output as a df"
else:
    raise Exception("Error: unrecognized input to PROCESS")

response = input(f"You are about to run this program with PROCESS={PROCESS} for {nfiles} files in {len(file_in_uris)} folder(s), which will {process_info}. Proceed? (Y/N) \n")
if response != "Y":
    raise Exception("Aborting process")

# if readonly, get list of operationids
if PROCESS == "read":
    # get list of operationids
    try:
        op_df = pd.read_csv(operationinfo_path)
    except:
        print("error opening operationinfo csv")

# iterating through folders
for i in range(len(file_in_uris)):
    print(f"Processing files in folder {file_in_uris[i]}...[{i+1}/{len(file_in_uris)}]")

    # case 1: full operation (wait = true)
    if PROCESS == "full":
        df_out = helper.batch_process_documents(project_id = project_id, 
                                                location = location, 
                                                processor_id = processor_id, 
                                                processor_version_id = processor_version_id, 
                                                gcs_input_prefix=file_in_uris[i],
                                                gcs_output_uri=file_out_uris[i],
                                                input_mime_type= mime_type,
                                                wait = True,
                                                operationinfo_path = operationinfo_path)
        outpathfull = out_prefix + intfolder + folder_roots[i] + out_suffix
        print(f"Writing data to local file {outpathfull}")
        df_out.to_csv(outpathfull)

    # case 2: writing only (running operations, wait = false)
    elif PROCESS == "write":
        while (helper.list_operations(project_id=project_id, location="us", operations_filter="TYPE=BATCH_PROCESS_DOCUMENTS AND STATE=RUNNING",
                                      getcount = True) >= 5):
            print(f"Currently at least 5 other active LROs -- will try again in {waittime} seconds")
            sleep(waittime)

        helper.batch_process_documents(project_id = project_id, 
                                        location = location, 
                                        processor_id = processor_id, 
                                        processor_version_id = processor_version_id, 
                                        gcs_input_prefix=file_in_uris[i],
                                        gcs_output_uri=file_out_uris[i],
                                        input_mime_type= mime_type,
                                        wait = False,
                                        operationinfo_path = operationinfo_path)
        
        # optional: list operations
        #helper.list_operations(project_id=project_id, location="us", operations_filter="TYPE=BATCH_PROCESS_DOCUMENTS AND STATE=RUNNING")
    
    # case 3: reading only (just call read function)
    elif PROCESS == "read":
        
        # getting most recent operation id corresp to in/out uri
        op_df_filt = op_df[(op_df.sourcepath == file_in_uris[i]) & (op_df.outpath == file_out_uris[i])]
        opname = str(op_df_filt.loc[op_df_filt.datetime.idxmax(), 'operationname'])
        print(opname)

        # getting output
        df_out = helper.batch_process_read(gcs_output_uri=file_out_uris[i], operation_name = opname)
        outpathfull = out_prefix + intfolder + folder_roots[i] + out_suffix
        print(f"Writing data to local file {outpathfull}")
        df_out.to_csv(outpathfull)
        
        # except ValueError:
        #     print("Error: (probably that) you are looking for an operation that has not been logged in this file")

    else:
        raise Exception("Error: unrecognized input to PROCESS")
    
