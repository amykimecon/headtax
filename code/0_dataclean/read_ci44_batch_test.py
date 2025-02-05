import documentai_helper as helper
from datetime import datetime
from google.cloud import documentai
from google.longrunning.operations_pb2 import GetOperationRequest

# initializing global variables for processor
project_id = "head-tax-ci44"
location = "us"
processor_id = "7db1369f50f3a46b"
mime_type = "image/jpeg"
processor_version_id = "pretrained-foundation-model-v1.3-2024-08-31"
#processor_version_id = "71a5ef1c133c5096"
bucket = "ci44_data"
folder = "test2"
file_in_uri = "gs://" + bucket + "/raw/" + folder + "/"
file_out_uri = "gs://" + bucket + "/clean/" + folder + "/"
startreel = 0 #16164 #set to 0 if testing
endreel = 16184
LISTLROs = True
READONLY = False
WAITSTATUS = False #default true (true if wait for the operation to finish running and return df)
operation_id = "14314377600048789958" # for testing/readonly only
out_prefix = '/Users/amykim/Princeton Dropbox/Amy Kim/head_tax/head_tax_data/cleaned/ci44/'
out_suffix = 'out_basemodel_' + datetime.now().strftime("%b%d") + '.csv'
operationinfo_path = out_prefix + 'operationinfo.csv'

if LISTLROs:
    helper.list_operations(project_id=project_id, location="us", operations_filter="TYPE=BATCH_PROCESS_DOCUMENTS AND STATE=RUNNING")
else:
    if READONLY:
        df_out = helper.batch_process_readonly(gcs_output_uri=file_out_uri, operation_id = operation_id)
        df_out.to_csv(out_prefix + folder + '/' + folder + out_suffix)
            
    else:
        if startreel < 16164:
            df_out = helper.batch_process_documents(project_id = project_id, 
                                                        location = location, 
                                                        processor_id = processor_id, 
                                                        processor_version_id = processor_version_id, 
                                                        gcs_input_prefix=file_in_uri,
                                                        #gcs_input_uri = file_in_uri,
                                                        gcs_output_uri=file_out_uri,
                                                        input_mime_type= mime_type,
                                                        wait = WAITSTATUS,
                                                        operationinfo_path = operationinfo_path)
            if WAITSTATUS:
                df_out.to_csv(out_prefix + folder + '/' + folder + out_suffix)

        else:
            for reelno in range(startreel, endreel + 1):
                tempfolder = "t" + str(reelno)
                file_in_uri = "gs://" + bucket + "/raw/" + tempfolder + "/"
                file_out_uri = "gs://" + bucket + "/clean/" + tempfolder + "/"
                df_out = helper.batch_process_documents(project_id = project_id, 
                                                        location = location, 
                                                        processor_id = processor_id, 
                                                        processor_version_id = processor_version_id, 
                                                        gcs_input_prefix=file_in_uri,
                                                        gcs_output_uri=file_out_uri,
                                                        input_mime_type= mime_type,
                                                        wait = WAITSTATUS,
                                                        operationinfo_path = operationinfo_path)
                df_out.to_csv(out_prefix + tempfolder + out_suffix)
