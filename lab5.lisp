;;; ---------------------------------------------------------------------------
;;; Утиліти для роботи з рядками та типами
;;; ---------------------------------------------------------------------------

(defun clean-string (str)
  "Видаляє пробіли, табуляцію та перенесення рядка з країв."
  (string-trim '(#\Space #\Tab #\Return #\Newline) str))

(defun split (line &optional (delimiter #\,))
  "Розбиває рядок на список підрядків за роздільником.
   Враховує, що між комами можуть бути пусті значення."
  (let ((result nil)
        (start 0))
    (loop for i = (position delimiter line :start start)
          do (if i
                 (progn
                   (push (clean-string (subseq line start i)) result)
                   (setf start (1+ i)))
                 (progn
                   (push (clean-string (subseq line start)) result)
                   (return))))
    (nreverse result)))

(defun try-parse-number (str)
  "Спроба перетворити рядок у число.
   Використовується при зчитуванні CSV, щоб числа не зберігалися як рядки."
  (if (string= str "")
      str
      (let ((val (parse-integer str :junk-allowed t)))
        ;; Перевірка: чи весь рядок є числом (щоб "12a" залишилося рядком)
        (if (and val (= (length (write-to-string val)) (length str)))
            val
            str))))

;;; ---------------------------------------------------------------------------
;;; Робота з файлами (IO)
;;; ---------------------------------------------------------------------------

(defun read-csv-to-alist (filename)
  "Зчитує CSV файл у список асоціативних списків.
   Перший рядок файлу вважається заголовком.
   Типи даних визначаються автоматично."
  (with-open-file (stream filename :direction :input)
    (let* ((header (split (clean-string (read-line stream)))) ; Читаємо ключі
           (alist nil))
      (do ((line (read-line stream nil 'eof) (read-line stream nil 'eof)))
          ((eq line 'eof) (nreverse alist))
        (let* ((values (split (clean-string line)))
               (record nil))
          ;; Формування одного запису
          (loop for key in header
                for value in values
                ;; Створюємо пару (Ключ . Значення) з правильною типізацією
                do (setq record (acons key (try-parse-number value) record)))
          (push (nreverse record) alist))))))

(defun write-alist-to-csv (data filename)
  "Записує список асоціативних списків у файл формату CSV."
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (when data
      ;; Формування заголовка з ключів першого запису
      (let* ((keys (mapcar #'car (first data)))
             (header (format nil "~{~A~^,~}" keys)))
        (format stream "~A~%" header)
        ;; Запис рядків даних
        (dolist (record data)
          (let ((line (format nil "~{~A~^,~}"
                              (mapcar (lambda (key) 
                                        (cdr (assoc key record :test #'string=))) 
                                      keys))))
            (format stream "~A~%" line)))))))

;;; ---------------------------------------------------------------------------
;;; Функція вибірки (Select)
;;; ---------------------------------------------------------------------------

(defun select (filename)
  "Повертає замикання (closure), яке містить дані файлу і дозволяє їх фільтрувати.
   Аргументи: шлях до файлу.
   Повертає: лямбда-функцію."
  ;; Оптимізація: файл читається лише 1 раз при виклику (select "file.csv")
  (let ((table (read-csv-to-alist filename)))
    (lambda (&rest filters)
      (if (null filters)
          table ;; Немає фільтрів -> повертаємо всю таблицю
          (remove-if-not
           (lambda (row)
             (cond
               ;; Варіант 1: Передано лямбду-предикат
               ((functionp (first filters))
                (funcall (first filters) row))
               ;; Варіант 2: Передано список пар ("Ключ" . Значення)
               (t
                (every (lambda (filter)
                         (let* ((key (car filter))
                                (required (cdr filter))
                                (actual (cdr (assoc key row :test #'string=))))
                           (if (listp required)
                               (member actual required :test #'equal)
                               (equal actual required))))
                       filters))))
           table)))))

;;; ---------------------------------------------------------------------------
;;; Конвертація типів (Пункт 5 - Виконано повністю)
;;; ---------------------------------------------------------------------------

(defun alist-to-hash-table (alist)
  "Конвертує асоціативний список в геш-таблицю."
  (let ((hash-table (make-hash-table :test #'equal)))
    (dolist (pair alist)
      (setf (gethash (car pair) hash-table) (cdr pair)))
    hash-table))

(defun hash-table-to-alist (hash-table)
  "Конвертує геш-таблицю назад у асоціативний список."
  (let ((alist nil))
    (maphash (lambda (k v)
               (push (cons k v) alist))
             hash-table)
    alist))

;;; ---------------------------------------------------------------------------
;;; Вивід
;;; ---------------------------------------------------------------------------

(defun print-table (records)
  "Форматований вивід таблиці в консоль."
  (when records
    (let* ((keys (mapcar #'car (first records)))
           (column-width 20))
      ;; Вивід назв колонок
      (dolist (key keys)
        (format t "~vA" column-width key))
      (format t "~%~A~%" (make-string (* column-width (length keys)) :initial-element #\-))
      ;; Вивід рядків
      (dolist (record records)
        (dolist (key keys)
          (format t "~vA" column-width (cdr (assoc key record :test #'string=))))
        (format t "~%"))))
   (format t "~%"))

;;; ---------------------------------------------------------------------------
;;; ТЕСТУВАННЯ
;;; ---------------------------------------------------------------------------

(defun test-check-database ()
  (format t "=== ПОЧАТОК ТЕСТУВАННЯ (Варіант: Українські Дрони) ===~%~%")

  ;; 1. Читання виробників
  (format t "--- 1. Всі виробники (з колонкою Country) ---~%")
  (print-table (funcall (select "manufacturers.csv")))

  ;; 2. Читання дронів
  (format t "--- 2. Всі дрони (з колонкою Price) ---~%")
  (print-table (funcall (select "drones.csv")))

  ;; 3. Фільтрація за значенням (Manufacturer = SkyUa)
  (format t "--- 3. Фільтр: Виробник 'SkyUa' ---~%")
  (print-table (funcall (select "drones.csv") '("Manufacturer" . "SkyUa")))
  
  ;; 4. Фільтрація лямбдою (Price > 700)
  (format t "--- 4. Фільтр: Ціна > 700$ (предикат) ---~%")
  (print-table (funcall (select "drones.csv")
          (lambda (row)
            (let ((price (cdr (assoc "Price($)" row :test #'string=))))
              (> price 700)))))

  ;; 5. Складна фільтрація
  (format t "--- 5. Складний фільтр: (Range 12 or 45) AND (KpiDrones or UkrWings) ---~%")
  (print-table (funcall (select "drones.csv") 
                        '("Flight_range(km)" . (12 45)) 
                        '("Manufacturer" . ("KpiDrones" "UkrWings"))))

  ;; 6. Запис у файл
  (format t "--- 6. Запис відфільтрованих даних у 'output.csv' ---~%")
  (let ((filtered (funcall (select "drones.csv") '("Manufacturer" . "SkyUa"))))
    (write-alist-to-csv filtered "output.csv")
    (format t "Дані про дрони SkyUa записано. Перевірте файл.~%"))

  ;; 7. Конвертація (Повне коло: Alist -> Hash -> Alist)
  (format t "--- 7. Перевірка конвертації (Alist <-> Hash) ---~%")
  (let* ((original-alist '(("ID" . 999) ("Name" . "TestDrone") ("Price" . 100)))
         (hash (alist-to-hash-table original-alist))   ;; В геш
         (restored (hash-table-to-alist hash)))        ;; Назад в список
    
    (format t "Оригінал: ~a~%" original-alist)
    (format t "З геш-таблиці (ID): ~a~%" (gethash "ID" hash))
    (format t "Відновлений список: ~a~%" restored)
    
    (if (eql (length original-alist) (length restored))
        (format t "УСПІХ: Конвертація пройшла коректно.~%")
        (format t "ПОМИЛКА: Втрачено дані при конвертації.~%")))

  (format t "~%=== ТЕСТУВАННЯ ЗАВЕРШЕНО ===~%"))

;; Запуск тестів
(test-check-database)