FUNCTION-POOL zmmsp02_batch.                "MESSAGE-ID ..

TABLES:cabn,klah.
* INCLUDE LZMMSP02_BATCHD...                 " Local class definition

data:g_characteristics  like  api_char occurs 0 with header line,
        g_attributes  like  api_ch_att occurs 0 with header line,
        g_values like api_value  occurs 0 with header line,
        g_subrc like sy-subrc.
data:gv_code TYPE sy-ucomm.
data:ok_code TYPE sy-ucomm.
