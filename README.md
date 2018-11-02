Mass Update Characteristics Value for Batch
============================================

Welcome to Mass Update Characteristics Value for Batch. This program is specific to SAP ABAP systems. It should work on any ECC release system, however some slight modifications may be needed for older SP levels.

## How to install
Method 1: by abapGit
Method 2: SE37 -> zmmsp02_fm_get_values
          SE37 -> zmmsp02_fm_char_ausp2out
          SE37 -> zmmsp02_fm_change_batch
          SE38 -> zmmsp02_batch_update (include GUI status/title/...)
          SE93 -> create ZMSC2N
## Test
1,Run T-code ZMSC2N.
2,Input material,batch,class...
3,Click execute button or F8,then go to next ALV screen.
4,Choose recordes which you want to update,and click Save button.

Wish you Good luck！

