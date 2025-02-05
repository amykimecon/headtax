import re
from typing import Optional, Sequence
import json
import os.path

from google.api_core.client_options import ClientOptions
from google.api_core.exceptions import InternalServerError
from google.api_core.exceptions import RetryError
from google.cloud import documentai  # type: ignore
from google.cloud import storage
from google.longrunning.operations_pb2 import GetOperationRequest
from google.longrunning.operations_pb2 import ListOperationsRequest  # type: ignore
from datetime import datetime

import pandas as pd

"""
BATCH PROCESSING
"""
# MAIN HELPER FOR BATCH PROCESSING -- initiates request, 
#   if wait = True then waits until request has completed and returns documents as pd df
#   if wait = False then just stores operation info in df
#   TODO: add check for if operation with same input/output folder is already running
def batch_process_documents(
    project_id: str,
    location: str,
    processor_id: str,
    gcs_output_uri: str,
    processor_version_id: Optional[str] = None,
    gcs_input_uri: Optional[str] = None,
    input_mime_type: Optional[str] = None,
    gcs_input_prefix: Optional[str] = None,
    field_mask: Optional[str] = None,
    wait = True,
    operationinfo_path = None,
    timeout: int = 100000,
) -> None:

    # creating and sending batch processing request to documentai API
    operation = batch_process_api(project_id=project_id,
                      location = location,
                      processor_id=processor_id,
                      gcs_output_uri=gcs_output_uri,
                      processor_version_id = processor_version_id,
                      gcs_input_uri=gcs_input_uri,
                      input_mime_type=input_mime_type,
                      gcs_input_prefix=gcs_input_prefix,
                      field_mask=field_mask,)
    
    print(f"Initiating operation {operation.operation.name}...")

    if wait:
        # Continually polls the operation until it is complete.
        # This could take some time for larger files
        try:
            print(f"Waiting for operation {operation.operation.name} to complete...")
            operation.result(timeout=timeout)
        # Catch exception when operation doesn't finish before timeout
        except (RetryError, InternalServerError) as e:
            print(e.message)

        # After the operation is complete,
        # get output document information from operation metadata
        metadata = documentai.BatchProcessMetadata(operation.metadata)

        if metadata.state != documentai.BatchProcessMetadata.State.SUCCEEDED:
            raise ValueError(f"Batch Process Failed: {metadata.state_message}")

        return(batch_process_read(gcs_output_uri=gcs_output_uri, 
                                      operation_name=operation.operation.name,
                                      bypass_check=True))

    else: #if not wait
        # stores operation info in df
        if operationinfo_path == None:
            print("Error: Invalid operationinfo_path")
            return(None)
        
        print("Storing operation info...")
        new_df = pd.DataFrame({'datetime': [datetime.now()],
                              'operationname': [operation.operation.name],
                              'sourcepath': [gcs_input_prefix],
                              'outpath': [gcs_output_uri]})
        
        if os.path.isfile(operationinfo_path):
            old_df = pd.read_csv(operationinfo_path)
            op_df = pd.concat([old_df, new_df], ignore_index = True)
        else:
            op_df = new_df
        
        op_df.to_csv(operationinfo_path)
        return(None)


# HELPER FOR DOCAI SIDE OF BATCH PROCESSING: takes in request and returns operation
def batch_process_api(
    project_id: str,
    location: str,
    processor_id: str,
    gcs_output_uri: str,
    processor_version_id: Optional[str] = None,
    gcs_input_uri: Optional[str] = None,
    input_mime_type: Optional[str] = None,
    gcs_input_prefix: Optional[str] = None,
    field_mask: Optional[str] = None,):

    client = documentai.DocumentProcessorServiceClient()

    if gcs_input_uri:
        # Specify specific GCS URIs to process individual documents
        gcs_document = documentai.GcsDocument(
            gcs_uri=gcs_input_uri, mime_type=input_mime_type
        )
        # Load GCS Input URI into a List of document files
        gcs_documents = documentai.GcsDocuments(documents=[gcs_document])
        input_config = documentai.BatchDocumentsInputConfig(gcs_documents=gcs_documents)
    else:
        # Specify a GCS URI Prefix to process an entire directory
        gcs_prefix = documentai.GcsPrefix(gcs_uri_prefix=gcs_input_prefix)
        input_config = documentai.BatchDocumentsInputConfig(gcs_prefix=gcs_prefix)

    # Cloud Storage URI for the Output Directory
    gcs_output_config = documentai.DocumentOutputConfig.GcsOutputConfig(
        gcs_uri=gcs_output_uri, field_mask=field_mask
    )

    # Where to write results
    output_config = documentai.DocumentOutputConfig(gcs_output_config=gcs_output_config)

    if processor_version_id:
        # The full resource name of the processor version, e.g.:
        # projects/{project_id}/locations/{location}/processors/{processor_id}/processorVersions/{processor_version_id}
        name = client.processor_version_path(
            project_id, location, processor_id, processor_version_id
        )
    else:
        # The full resource name of the processor, e.g.:
        # projects/{project_id}/locations/{location}/processors/{processor_id}
        name = client.processor_path(project_id, location, processor_id)

    request = documentai.BatchProcessRequest(
        name=name,
        input_documents=input_config,
        document_output_config=output_config,
    )

    # BatchProcess returns a Long Running Operation (LRO)
    operation = client.batch_process_documents(request)

    return(operation)


# CLEANING OUTPUT OF BATCH PROCESS -- takes in operation name and gs location where output was stored and returns df of docai output
def batch_process_read(
    gcs_output_uri: str,
    operation_name: str,
    bypass_check = False, #whether or not to bypass checking if operation is complete -- NEVER MANUALLY SET TO TRUE UNLESS THROUGH batch_process_documents
) -> None:

    storage_client = storage.Client()

    # list of dfs for output
    dfoutlist = [] 
    filelist = set()

    output_bucket, output_prefix_stub = re.match(r"gs://(.*?)/(.*)", gcs_output_uri).groups()
    operation_id = re.match(r".*operations/([0-9]+)", operation_name).group(1) 
    output_prefix = output_prefix_stub + operation_id + "/"
    output_blobs = storage_client.list_blobs(output_bucket, prefix=output_prefix)
    
    if not bypass_check:
        # check if operation is complete
        client = documentai.DocumentProcessorServiceClient()
        request = GetOperationRequest(name=operation_name)
        operation = client.get_operation(request)

        if not operation.done:
            raise Exception(f"Error: Operation {operation_id} not complete")
        
        #metadata = documentai.BatchProcessMetadata(operation.metadata)

        # if metadata.state != documentai.BatchProcessMetadata.State.SUCCEEDED:
        #     raise ValueError(f"Batch Process Failed: {metadata.state_message}")
        # the above stuff is throwing an error so temp solution
        if str(operation.error) != "":
            raise ValueError(f"Batch Process Failed: {operation.error}")

    # Document AI may output multiple JSON files per source file
    for blob in output_blobs:
        # Document AI should only output JSON files to GCS
        if blob.content_type != "application/json":
            print(
                f"Skipping non-supported file: {blob.name} - Mimetype: {blob.content_type}"
            )
            continue

        # extracting only entities and converting to pandas df
        pd_df = read_json(blob)

        # checking if filename already exists
        if blob.name in filelist:
            print("file" + blob.name + " already read")
        elif not isinstance(pd_df, pd.DataFrame):
            print("No entities to read, skipping this file")
        else:
            # if not, adding df to list of dfs and adding filename to list of filenames
            dfoutlist.append(pd_df)

            # adding filename to set of filenames
            filelist.add(blob.name)

    dfout = pd.concat(dfoutlist, axis = 0, ignore_index=True)
    return(dfout)
                    
"""
PROCESS HELPERS
"""
# LISTING LROs
def list_operations(
    project_id: str, location: str, operations_filter: str, getcount = False,
) -> None:
    
    client = documentai.DocumentProcessorServiceClient()

    # Format: `projects/{project_id}/locations/{location}`
    name = client.common_location_path(project=project_id, location=location)
    request = ListOperationsRequest(
        name=f"{name}/operations",
        filter=operations_filter,
    )

    # Make ListOperations request
    operations = client.list_operations(request=request)

    # Print the Operation Information
    operationslist = operations.operations

    if getcount:
        return(len(list(operationslist)))
    else:
        for operation in operationslist:
            print(operation.name)
        print(f"Total number of ongoing operations: {len(list(operationslist))}")
        
"""
UTILITY HELPERS
"""
# regex helpers: get reel/img number
def extract_no(filename: str, # filename to parse
               flag: str # flag to indicate what number to extract (either img or reel)
               ):
    search_regex = flag + "([0-9]+)"
    num = re.search(search_regex, filename)
    if num:
        return num.group(1)
    else:
        print("Error with reading " + flag + " number from filename")

# read raw json file from docai output as pandas df
def read_json(blob):
    # downloading raw json file
    json_raw = json.loads(blob.download_as_bytes())

    # extracting only entities and converting to pandas df
    if not "entities" in json_raw:
        return(None)
    
    pd_df = pd.json_normalize(json_raw['entities'])

    # extracting img and reel numbers from file name and adding to df
    pd_df['reel'] = extract_no(blob.name, "reel")
    pd_df['img'] = extract_no(blob.name, "img")
    pd_df['filename'] = blob.name

    return pd_df

"""
DEPRECATED/NOT IN USE
"""
# # MAIN HELPER FOR SINGLE DOCUMENT 
# def process_document_custom_extractor_sample(
#     project_id: str,
#     location: str,
#     processor_id: str,
#     processor_version: str,
#     file_path: str,
#     mime_type: str,
# ) -> None:

#     # Online processing request to Document AI
#     document = process_document(
#         project_id,
#         location,
#         processor_id,
#         processor_version,
#         file_path,
#         mime_type,
#     )

#     for entity in document.entities:
#         print_entity(entity)
#         # Print Nested Entities (if any)
#         for prop in entity.properties:
#             print_entity(prop)


# def print_entity(entity: documentai.Document.Entity) -> None:
#     # Fields detected. For a full list of fields for each processor see
#     # the processor documentation:
#     # https://cloud.google.com/document-ai/docs/processors-list
#     key = entity.type_

#     # Some other value formats in addition to text are available
#     # e.g. dates: `entity.normalized_value.date_value.year`
#     text_value = entity.text_anchor.content or entity.mention_text
#     confidence = entity.confidence
#     normalized_value = entity.normalized_value.text
#     print(f"    * {repr(key)}: {repr(text_value)} ({confidence:.1%} confident)")

#     if normalized_value:
#         print(f"    * Normalized Value: {repr(normalized_value)}")

# def process_document(
#     project_id: str,
#     location: str,
#     processor_id: str,
#     processor_version: str,
#     file_path: str,
#     mime_type: str,
#     process_options: Optional[documentai.ProcessOptions] = None,
# ) -> documentai.Document:
#     # You must set the `api_endpoint` if you use a location other than "us".
#     client = documentai.DocumentProcessorServiceClient(
#     )

#     # The full resource name of the processor version, e.g.:
#     # `projects/{project_id}/locations/{location}/processors/{processor_id}/processorVersions/{processor_version_id}`
#     # You must create a processor before running this sample.
#     name = client.processor_version_path(
#         project_id, location, processor_id, processor_version
#     )

#     # Read the file into memory
#     with open(file_path, "rb") as image:
#         image_content = image.read()

#     # Configure the process request
#     request = documentai.ProcessRequest(
#         name=name,
#         raw_document=documentai.RawDocument(content=image_content, mime_type=mime_type),
#         # Only supported for Document OCR processor
#         # process_options=process_options,
#     )

#     result = client.process_document(request=request)

#     # For a full list of `Document` object attributes, reference this page:
#     # https://cloud.google.com/document-ai/docs/reference/rest/v1/Document
#     return result.document
