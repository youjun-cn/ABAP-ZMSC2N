Mass Update Characteristics Value for Batch
============================================

Welcome to Mass Update Characteristics Value for Batch. This program is specific to SAP ABAP systems. It should work on any ECC release system, however some slight modifications may be needed for older SP levels.

## How to install
<br>Method 1: create it by abapGit tools.
<br>Method 2: create it manually.
          <br>SE37 -> zmmsp02_fm_get_values
          <br>SE37 -> zmmsp02_fm_char_ausp2out
          <br>SE37 -> zmmsp02_fm_change_batch
          <br>SE38 -> zmmsp02_batch_update (include GUI status/title/...)
          <br>SE93 -> create ZMSC2N
## Test
<br>1,Run T-code ZMSC2N.
<br>2,Input material,batch,class...
<br>3,Click execute button or F8,then go to next ALV screen.
<br>4,Choose recordes which you want to update,and click Save button.

<br>Wish you good luck！

