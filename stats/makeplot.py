import sys

AC_SPACE = 5
AC_CONTENT = "fails,solutions,tot time,"\
    "backtrack time,revise backtrack"
ALGO_NB = 4


def get_line_cnt(line):
    return line.split(" ")[-1].rstrip()


def read_file(file_path):
    with open(file_path) as f:
        res = []
        algo_names = []

        for i in range(ALGO_NB):
            res.append([])

        lines = f.readlines()
        program = lines[0].rstrip().replace(" ", "")

        cnt = list(map(get_line_cnt, lines))
        coeff = ALGO_NB * (AC_SPACE + 3) + 1

        for i in range(0, len(cnt)//coeff):
            algo = i * coeff + 1
            for algo_nb in range(ALGO_NB):

                algo_name = algo + 1 + algo_nb * (AC_SPACE + 3)

                if len(algo_names) < ALGO_NB:
                    algo_names.append(cnt[algo_name])
                prov = []

                for p in range(AC_CONTENT.count(",") + 1):
                    prov.append(cnt[algo_name + 2 + p])
                res[algo_nb].append(prov)

        # print(program)
        for algo, i in zip(algo_names, res):
            output = open(program+"-"+algo+".csv", "w")
            # print(algo)
            output.write("x," + AC_CONTENT+"\n")
            # print(AC_CONTENT)
            for pos, j in enumerate(i):
                output.write(str(pos + 1) + "," + ",".join(j) + "\n")
            # print()


if __name__ == "__main__":
    # print(sys.argv)
    read_file(sys.argv[1])
