import os
import pandas as pd
from typing import List

# importing helper functions
os.chdir("/Users/amykim/Documents/GitHub/headtax/code")
import documentai_helper as docai

# initializing global variables for processor
project_id = "head-tax-ci44"
location = "us"
processor_id = "6bf65172dff14c90"
mime_type = "image/jpeg"
processor_version_id = "pretrained-form-parser-v2.0-2022-11-10"
file_in_root = "/Users/amykim/Dropbox (Princeton)/head_tax/head_tax_data/raw/ci44"
file_out_root = "/Users/amykim/Dropbox (Princeton)/head_tax/head_tax_data/cleaned"

file_path = file_in_root + "/t16165/" + "img182.jpg"
doc = docai.process_document(project_id = project_id, 
                                                location = location, 
                                                processor_id = processor_id, 
                                                processor_version_id = processor_version_id, 
                                                file_path = file_path,
                                                mime_type = mime_type)

for page in doc.pages:
    print("page" + str(page.page_number))
    for field in page.form_fields:
        name = docai.layout_to_text(field.field_name, doc.text)
        value = docai.layout_to_text(field.field_value, doc.text)
        print(f"    * {repr(name.strip())}: {repr(value.strip())}")


# df.columns = header_row_values
# print(df)
# df.to_csv(file_out_root + "/ci44/t16165/img48.csv", index = False)

# # TEACHER COUNT DATA
# for year in range(1930, 1938):
#     print(year)
#     if year != 1931:
#         for pg in range(1,6):
#             for pt in ["1","2"]:
#                 file_path = file_in_root + "counts/intermediate/" + str(year) + "_p" + str(pg) + "_pt" + pt + ".png"
#                 (df, header_row_values) = docai.make_df(project_id, location, processor_id, processor_version_id, file_path, mime_type)

#                 print(df.shape)
#                 #print(df)
#                 if df.size != 0:
#                     df.columns = header_row_values
#                     df.to_csv(file_out_root + "counts/ocr/" + str(year) + "_p" + str(pg) + "_pt" + pt + ".pdf", index = False)
#     else:
#         for pg in range(1,6):
#             file_path = file_in_root + "counts/processed/" + str(year) + "_p" + str(pg) + ".png"
#             (df, header_row_values) = docai.make_df(project_id, location, processor_id, processor_version_id, file_path, mime_type)

#             print(df.shape)
#             #print(df)

#             df.columns = header_row_values
#             df.to_csv(file_out_root + "counts/ocr/" + str(year) + "_p" + str(pg) + ".pdf", index = False)



