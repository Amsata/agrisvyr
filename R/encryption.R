#' Read password based encrypted file
#'
#' @param path
#' @param password
#' @param rounds
#' @param size
#' @importFrom openssl bcrypt_pbkdf
#' @importFrom openssl aes_cbc_decrypt
#'
#' @return
#' @export
#'
#' @examples
read_enc=function(path,password,rounds=100L,size=32L) {
  enc <- readRDS(path)
  key <- openssl::bcrypt_pbkdf(password = charToRaw(password),salt = enc$salt,rounds = rounds,size = size)
  decrypted_df <- openssl::aes_cbc_decrypt(enc$data, key = key)
  decrypted_df <- unserialize(decrypted_df)
  return(decrypted_df)
}


#' Save password based encrypted file
#'
#' @param df
#' @param path
#' @param password
#' @param rounds
#' @param size
#' @importFrom openssl serialize
#' @importFrom openssl rand_bytes
#' @importFrom openssl bcrypt_pbkdf
#' @importFrom openssl aes_cbc_encrypt
#'
#' @return
#' @export
#'
#' @examples
write_enc=function(df,path,password,rounds=100L,size=32L) {
  raw_data <- serialize(df, NULL)
  salt <- openssl::rand_bytes(16)
  key <- bcrypt_pbkdf(password =charToRaw(password),salt = salt,rounds = rounds,size = size)
  encrypted <- openssl::aes_cbc_encrypt(raw_data, key = key)
  saveRDS(list(salt = salt, data = encrypted), path)

}
