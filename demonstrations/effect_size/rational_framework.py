import csv
import numpy as np
import math
import matplotlib.pyplot as plt
import random

import pandas as pd
from scipy.stats import norm

# Constants and data initialization
n_data = 0
eps = 1e-5

rand_size = 160 * 2

n_ground_truth = 0
condition_dict = {
    "densities": 0,
    "intervals": 1,
    "HOPs": 2,
    "QDPs": 3
}

means_dict = {
    "FALSE": 0,
    "TRUE": 1
}
'''
lo_ground_truth_dict = {
	"0.200670695462151": 0,
	"0.451517615553149": 1,
	"0.699153204620157": 2,
	"0.949427355463583": 3
}
'''
lo_ground_truth_dict = {}

trial_groundtruth_dict = {}
trial_means_dict = {}
trial_condition_dict = {}

sd_diff_dict = {}
n_sd_diff = 0

lo_ground_truth = []
means = ["FALSE", "TRUE"]
condition = ["densities",
             "intervals",
             "HOPs",
             "QDPs"]

decision_truth_map = {}


def inv_log(a):
    # Computes the inverse of the logit function
    return 1.00 / (1.00 + math.exp(0.00 - float(a)))


def qlogis(a):
    # Computes the logit function
    return math.log(a / (1.00 - a))


def heuristic(ground_truth, sd_diff):
    # Computes the heuristic given ground truth and standard deviation difference
    return 0.5 + float(ground_truth) * sd_diff / 225.0


# Load PoS data

## condition, means, groundtruth
behavioral_bins = np.arange(100, dtype=float)

freq_heuristic = np.zeros((2, 4, 2, 8, 100), dtype=float)

ground_truth_data = []
pred_data = []
means_data = []
condition_data = []

with open('./data/posterior_predictive_draws_prob_superiority.csv', newline='') as f:
    data = csv.reader(f)
    headers = next(data)
    for row in data:
        if not (row[0] in lo_ground_truth_dict):
            # read ground truth pos row[0] in logit space
            lo_ground_truth_dict[row[0]] = n_ground_truth
            # compute probability of winning: first inv_log, then compute p[winning] from pos
            lo_ground_truth.append(norm.cdf(math.sqrt(2) * norm.ppf(inv_log(row[0]))))
            n_ground_truth += 1
        ground_truth_data.append(lo_ground_truth_dict[row[0]])
        pred_data.append(row[10])
        means_data.append(row[1])
        condition_data.append(row[3])
        n_data += 1


def binning(bin_size):
    # Computes the empirical distribution for each combination of condition, mean and ground truth
    freq = np.zeros((4, 2, 8, 100), dtype=float)
    for i in range(n_data):
        pred = int(inv_log(pred_data[i]) / bin_size)
        # counting condition, means, groundtruth, the response numbers, empirical distribution
        freq[condition_dict[condition_data[i]], means_dict[means_data[i]], ground_truth_data[i], pred] += 1
    return freq


bin_size = 0.02
freq = binning(bin_size)
# compute prior - np.sum(freq[0, 0], axis = 1) is the number of trials with each ground truth, lo_ground_truth winning prob
prior_freq = np.inner(np.sum(freq[0, 0], axis=1), lo_ground_truth) / np.sum(freq[0, 0])


def compute_freq(bin_num, bin_size):
    # Computes the frequency given bin number and bin size
    return (bin_num + 0.5) * bin_size


def eq_err(a, b):
    # Compares if two values are equal within a margin of error 'eps'
    if abs(a - b) <= eps:
        return True
    return False


def log(a):
    # Computes the natural logarithm with an added epsilon to avoid log(0)
    return np.log(a + eps)


def expect_quadratic(report, pos_truth):
    # Computes the expected quadratic score given a report and the position of the ground truth
    if type(pos_truth) == 'int':
        tmp_ground_truth = [lo_ground_truth[pos_truth]]
    else:
        tmp_ground_truth = [lo_ground_truth[i] for i in pos_truth]
    tmp_ground_truth = np.array(tmp_ground_truth)
    # decision score
    return decision_score(np.less_equal(prior_freq, report), tmp_ground_truth)


def decision_score(decision, truth):
    # Computes the decision score given a decision and the ground truth
    return decision * (3.17 * truth - 1.00) + (1 - decision) * 3.17 * 0.5


def rand_draw(freq):
    # Draws samples from the frequency distribution
    lst = list(range(0, 100))
    sample = random.choices(lst, weights=freq, k=rand_size)
    freq_tmp = np.zeros((1, 100))
    for i in sample:
        freq_tmp[0, i] += 1
    return freq_tmp


def calc_score(freq_tmp, bin_size):
    # Calculates behavioral and calibrated behavioral scores given frequency and bin size
    behavioral_score = 0.0
    for i in range(n_ground_truth):
        behavioral_score += np.inner(freq_tmp[i],
                                     expect_quadratic(compute_freq(behavioral_bins, bin_size), [i])) / np.sum(freq_tmp)

    cond_posterior = np.zeros((n_ground_truth, 100), dtype=float)
    for i in range(n_ground_truth):
        for j in range(100):
            if eq_err(freq_tmp[i, j], 0.0) != True:
                cond_posterior[i, j] = freq_tmp[i, j] / np.sum(freq_tmp[:, j])
    calib_posterior = np.matmul(lo_ground_truth, cond_posterior)
    calib_behav_score = 0.00
    for i in range(n_ground_truth):
        calib_behav_score += np.inner(freq_tmp[i], expect_quadratic(calib_posterior, [i])) / np.sum(freq_tmp)
    return behavioral_score, calib_behav_score


def main():
    # Main function to run the analysis
    prior_score = np.inner(np.sum(freq[0, 0], axis=1),
                           expect_quadratic(prior_freq, list(range(0, n_ground_truth)))) / np.sum(freq[0, 0])
    print("prior freq", prior_freq)
    print("prior score: ", prior_score)

    posterior_score = np.inner(np.sum(freq[0, 0], axis=1),
                               expect_quadratic(lo_ground_truth, list(range(0, n_ground_truth)))) / np.sum(
        np.sum(freq[0, 0]))
    print(lo_ground_truth)
    print(expect_quadratic(lo_ground_truth, list(range(0, n_ground_truth))))
    print("posterior score: ", posterior_score)

    score_ind = []
    score_behav = []
    score_calib = []

    decision_ground_truth_dict = {}
    decision_ground_truth_list = []
    decision_ground_truth_count = 0
    decision_freq = np.zeros((4, 2, 8, 2), dtype=float)

    with open('./data/posterior_predictive_draws_decisions.csv', newline='') as f:
        data = csv.reader(f)
        headers = next(data)
        for row in data:
            if not row[0] in decision_ground_truth_dict:
                tmp_ground_truth = inv_log(float(row[0]) + qlogis(0.5 + 1.0 / 3.17))
                flag = True
                for i in range(len(decision_ground_truth_list)):
                    t = decision_ground_truth_list[i]
                    if eq_err(t, tmp_ground_truth):
                        flag = False
                        decision_ground_truth_dict[row[0]] = i
                if flag:
                    decision_ground_truth_list.append(tmp_ground_truth)
                    decision_ground_truth_dict[row[0]] = decision_ground_truth_count
                    decision_ground_truth_count += 1
            decision_freq[
                condition_dict[row[3]], means_dict[row[1]], decision_ground_truth_dict[row[0]], int(row[10])] += 1
            
    # Calculate scores
    score_decision = []
    n_round = 100
    for t in range(4):
        for m in range(2):
            tmp_decision = []
            for i in range(n_round):
                score_tmp = 0.0
                for j in range(0, decision_ground_truth_count):
                    sample = random.choices([0, 1], weights=decision_freq[t, m, j], k=rand_size)
                    freq_tmp = float(sum(sample)) / rand_size
                    if t == 0 and m == 0 and i == 0:
                        print(freq_tmp)
                    score_tmp += decision_score(freq_tmp, decision_ground_truth_list[j])
                score_tmp = score_tmp / decision_ground_truth_count
                tmp_decision.append(score_tmp)
                if t == 0 and m == 0 and i == 0:
                    print(score_tmp)
            score_decision.extend(tmp_decision)

    score_bounds = [prior_score, posterior_score]
    score_bounds_ind = [0, 0]

    for t in range(4):
        for m in range(2):
            tmp_behav = []
            tmp_calib = []
            for i in range(n_round):
                freq_tmp = rand_draw(freq[t, m, 0])
                for j in range(1, n_ground_truth):
                    freq_tmp = np.append(freq_tmp, rand_draw(freq[t, m, j]), axis=0)
                behav_tmp, calib_tmp = calc_score(freq_tmp, bin_size)
                tmp_behav.append(behav_tmp)
                tmp_calib.append(calib_tmp)

            score_ind.extend([t * 4 + m + 1] * n_round)
            score_behav.extend(tmp_behav)
            score_calib.extend(tmp_calib)
    
    # Save results to csv files

    all_behavioral = pd.DataFrame({
        "vis": [condition[(index - 1) // 4] for index in score_ind],
        "mean": [means[(index - 1) % 4] for index in score_ind],
        "behavioral": score_decision,
        "calibrated_behavioral": score_calib,
        "calibrated_behavioral_updating": score_behav
    })
    
    all_rational = pd.DataFrame({
        "rational": score_bounds
    })
    
    all_behavioral.to_csv("./data/all_behavioral.csv")
    all_rational.to_csv("./data/all_rational.csv")

main()