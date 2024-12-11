from typing import Optional, Sequence, List
from google.api_core.client_options import ClientOptions
from google.cloud import documentai  # type: ignore
import pandas as pd

def make_df(project_id, location, processor_id, processor_version_id, file_path, mime_type):
    doc = process_document(project_id = project_id, 
                                                location = location, 
                                                processor_id = processor_id, 
                                                processor_version_id = processor_version_id, 
                                                file_path = file_path,
                                                mime_type = mime_type)
    print(doc.text)
    header_row_values: List[List[str]] = []
    body_row_values: List[List[str]] = []

    for page in doc.pages:
        for table in page.tables:
            header_row_values = get_table_data(table.header_rows, doc.text)
            body_row_values = get_table_data(table.body_rows, doc.text)

    # Create a Pandas Dataframe to print the values in tabular format.
    df = pd.DataFrame(
        data=body_row_values
    )
    return (df, header_row_values)

def process_document(
    project_id: str,
    location: str,
    processor_id: str,
    processor_version_id: str,
    file_path: str,
    mime_type: str,
    process_options: Optional[documentai.ProcessOptions] = None,
) -> documentai.Document:
    
    client = documentai.DocumentProcessorServiceClient(
        client_options=ClientOptions(
            api_endpoint=f"{location}-documentai.googleapis.com"
        )
    )

    name = client.processor_version_path(
        project_id, location, processor_id, processor_version_id
    )

    # Read the file into memory
    with open(file_path, "rb") as image:
        image_content = image.read()

    # Configure the process request
    request = documentai.ProcessRequest(
        name=name,
        raw_document=documentai.RawDocument(content=image_content, mime_type=mime_type),
        process_options=process_options,
    )

    result = client.process_document(request=request)

    return result.document

def get_table_data(
    rows: Sequence[documentai.Document.Page.Table.TableRow], text: str
) -> List[List[str]]:
    """
    Get Text data from table rows
    """
    all_values: List[List[str]] = []
    for row in rows:
        current_row_values: List[str] = []
        for cell in row.cells:
            current_row_values.append(
                text_anchor_to_text(cell.layout.text_anchor, text)
            )
        all_values.append(current_row_values)
    return all_values

def text_anchor_to_text(text_anchor: documentai.Document.TextAnchor, text: str) -> str:
    """
    Document AI identifies table data by their offsets in the entirity of the
    document's text. This function converts offsets to a string.
    """
    response = ""
    # If a text segment spans several lines, it will
    # be stored in different text segments.
    for segment in text_anchor.text_segments:
        start_index = int(segment.start_index)
        end_index = int(segment.end_index)
        response += text[start_index:end_index]
    return response.strip().replace("\n", " ")


def layout_to_text(layout: documentai.Document.Page.Layout, text: str) -> str:
    """
    Document AI identifies text in different parts of the document by their
    offsets in the entirety of the document"s text. This function converts
    offsets to a string.
    """
    # If a text segment spans several lines, it will
    # be stored in different text segments.
    return "".join(
        text[int(segment.start_index) : int(segment.end_index)]
        for segment in layout.text_anchor.text_segments
    )
