package ${1:`(mapconcat 'identity (split-string (replace-regexp-in-string ".*src\\(/\\(main\\|test\\)\\)?\\(/java\\)?" "" default-directory) "/" t) ".")`};

/**
 * @author Seong Yong-ju <sei40kr@gmail.com>
 */
public class `(file-name-base (or (buffer-file-name) (buffer-name)))` {
  $0
}
