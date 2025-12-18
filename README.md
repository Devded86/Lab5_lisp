<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 5</b><br/>
"Робота з базою даних"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студент</b>: Марчук Дмитро КВ-22</p>
<p align="right"><b>Рік</b>: 2025</p>

## Загальне завдання
В роботі необхідно реалізувати утиліти для роботи з базою даних, заданою за варіантом
(п. 5.1.1). База даних складається з кількох таблиць. Таблиці представлені у вигляді CSV
файлів. При зчитуванні записів з таблиць, кожен запис має бути представлений певним
типом в залежності від варіанту: структурою, асоціативним списком або геш-таблицею.

1. Визначити структури та/або утиліти для створення записів з таблиць (в залежності
від типу записів, заданого варіантом).
2. Розробити утиліту(-и) для зчитування таблиць з файлів. Значення колонок мають
бути розібрані відповідно до типу даних у них. Наприклад, рядок — це просто
рядок; числові колонки необхідно розібрати як цілі числа або числа з рухомою
крапкою.
3. Розробити функцію select , яка отримує на вхід шлях до файлу з таблицею, а
також якийсь об'єкт, який дасть змогу зчитати записи конкретного типу або
структури. Це може бути ключ, список з якоюсь допоміжною інформацією, функція і
т. і. За потреби параметрів може бути кілька. select повертає лямбда-вираз,
який, в разі виклику, виконує "вибірку" записів з таблиці, шлях до якої було
передано у select . При цьому лямбда-вираз в якості ключових параметрів може
отримати на вхід значення полів записів таблиці, для того щоб обмежити вибірку
лише заданими значеннями (виконати фільтрування). Вибірка повертається у
вигляді списку записів.
4. Написати утиліту(-и) для запису вибірки (списку записів) у файл.
5. Написати функції для конвертування записів у інший тип (в залежності від
варіанту):
структури у геш-таблиці
геш-таблиці у асоціативні списки
асоціативні списки у геш-таблиці
6. Написати функцію(-ї) для "красивого" виводу записів таблиці (pretty-print).

   
## Варіант 3 (15)
База даних: Виробництво дронів

Тип записів: Асоціативний список

Таблиці: Виробники дронів, Дрони
  
## Лістинг реалізації завдання
```lisp
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
        ;; Перевірка: чи весь рядок є числом (щоб уникнути часткового парсингу)
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
;;; Конвертація типів (Пункт 5)
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
```
  
### Тестові набори та утиліти
```lisp
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
```

### Тестування
```lisp
CL-USER> (test-check-database)
=== ПОЧАТОК ТЕСТУВАННЯ (Варіант: Українські Дрони) ===

--- 1. Всі виробники (з колонкою Country) ---
Manufacturer_ID     Manufacturer_Name   Country             
------------------------------------------------------------
101                 SkyUa               Ukraine             
102                 KpiDrones           Ukraine             
103                 UkrWings            Ukraine             
104                 GlobalHawk          USA                 

--- 2. Всі дрони (з колонкою Price) ---
Drone_ID            Drone_name          Manufacturer        Flight_range(km)    Price($)            
----------------------------------------------------------------------------------------------------
1001                Viy                 SkyUa               10                  500                 
1002                Molfar              KpiDrones           12                  600                 
1003                Shark               UkrWings            45                  3000                
1004                Reaper              GlobalHawk          1000                1500000             

--- 3. Фільтр: Виробник 'SkyUa' ---
Drone_ID            Drone_name          Manufacturer        Flight_range(km)    Price($)            
----------------------------------------------------------------------------------------------------
1001                Viy                 SkyUa               10                  500                 

--- 4. Фільтр: Ціна > 700$ (предикат) ---
Drone_ID            Drone_name          Manufacturer        Flight_range(km)    Price($)            
----------------------------------------------------------------------------------------------------
1003                Shark               UkrWings            45                  3000                
1004                Reaper              GlobalHawk          1000                1500000             

--- 5. Складний фільтр: (Range 12 or 45) AND (KpiDrones or UkrWings) ---
Drone_ID            Drone_name          Manufacturer        Flight_range(km)    Price($)            
----------------------------------------------------------------------------------------------------
1002                Molfar              KpiDrones           12                  600                 
1003                Shark               UkrWings            45                  3000                

--- 6. Запис відфільтрованих даних у 'output.csv' ---
Дані про дрони SkyUa записано. Перевірте файл.
--- 7. Перевірка конвертації (Alist <-> Hash) ---
Оригінал: ((ID . 999) (Name . TestDrone) (Price . 100))
З геш-таблиці (ID): 999
Відновлений список: ((Price . 100) (Name . TestDrone) (ID . 999))
УСПІХ: Конвертація пройшла коректно.

=== ТЕСТУВАННЯ ЗАВЕРШЕНО ===
```
