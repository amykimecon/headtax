import documentai_helper as helper

# initializing global variables for processor
project_id = "head-tax-ci44"
location = "us"
processor_id = "7db1369f50f3a46b"
mime_type = "image/jpeg"
processor_version_id = "pretrained-foundation-model-v1.3-2024-08-31"
#processor_version_id = "71a5ef1c133c5096"
file_in_root = "/Users/amykim/Dropbox (Princeton)/head_tax/head_tax_data/raw/ci44"
file_out_root = "/Users/amykim/Dropbox (Princeton)/head_tax/head_tax_data/cleaned"

file_path = file_in_root + "/t16169/" + "reel16169_img594.jpg"
doc = helper.process_document_custom_extractor_sample(project_id = project_id, 
                                                location = location, 
                                                processor_id = processor_id, 
                                                processor_version = processor_version_id, 
                                                file_path = file_path,
                                                mime_type = mime_type)
