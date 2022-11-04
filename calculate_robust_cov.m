data = readtable('daily_return.csv');
 
idx_post10 = 253;
idx_post09 = 232;
idx_post08 = 209;
idx_post07 = 189;
idx = [idx_post10];%[idx_post07 idx_post08 idx_post09 idx_post10];
month = 7;
for iiidx = idx
    A = zeros(10,10);
    for i = 1:10
        for j = 1:10
            if i == j
                A(i,j) = 1;
            else
                A(i,j) = corr(table2array(data(iiidx-63:iiidx,i+1)), table2array(data(iiidx-63:iiidx,j+1)), 'rows','complete');
            end
        end
    end

    A = (A + A') / 2;
    cormat = nearcorr(A);
    
    sigma = zeros(10,1);
    for i = 1:10
        sigma(i) = nanstd(table2array(data(:,i+1)));
    end
    sigma = diag(sigma);
    
    covmat = sigma * cormat * sigma;
    
    B = cov(table2array(data(:,i+1:end)), 'omitrows');
    
    %writematrix(covmat,strcat('robust_cov_upto', num2str(month), '.csv'));
    month = month+1;
end


names = convertCharsToStrings(data.Properties.VariableNames);
names = names(2:end);
ii = ones(size(cormat));
idx = tril(ii);
cormat(~idx) = nan;
heatmap(names,names,cormat, 'MissingDataColor', 'w', 'GridVisible', 'off', 'MissingDataLabel', " ")
%heatmap(names,names,cormat)