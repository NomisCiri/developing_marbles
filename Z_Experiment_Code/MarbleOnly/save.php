<?php $data = json_encode(json_decode($_POST['filedata']), JSON_PRETTY_PRINT);
// write the file to disk
file_put_contents($filename, $data); ?>