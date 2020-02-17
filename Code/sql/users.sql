CREATE USER 'hmagee'@'%' IDENTIFIED BY 'hmagee';
CREATE USER 'mholswade'@'%' IDENTIFIED BY 'mholswade';
CREATE USER 'hammerling'@'%' IDENTIFIED BY 'hammerling';
CREATE USER 'lblake'@'%' IDENTIFIED BY 'lblake';
CREATE USER 'dykstal'@'%' IDENTIFIED BY 'dykstal';

GRANT SELECT, INSERT, UPDATE, ALTER, CREATE, DELETE, DROP, INDEX, REFERENCES
ON tropomi.* TO
    'hmagee'@'%',
    'mholswade'@'%',
    'hammerling'@'%',
    'lblake'@'%',
    'dykstal'@'%';

